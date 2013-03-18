{-
This is a fusion of the OIS and OGRE modules
-}
module Reactive.Banana.BOGRE (
        
        
        HookedBogreSystem,
        GameBuilder,

        runGame,
        stopBogre,

        setPosB,
        setVelB,
        
        getPositionB,
        
        frameE,
        frameT,

        addEntity,
        createNodeOnE,
        
        getDynamicDelayedPosBs,
        setDynamicPositionBs,
        setDynamicVelBs,
        sphereCollisionsE,

        getMousePosB,
        getMouseVelB,
        getKeyStateE,
        getKeyDownE,
        getKeyUpE,
        getTimeB,
        getRandomB,
        getRandomVec3B,

        velocityToPositionB
        
        
) where



import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.OIS
import Reactive.Banana.OGRE hiding (addEntity)
import qualified Reactive.Banana.OGRE as OGRE

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import BB.Workarounds

import BB.Util.Vec

import System.Random hiding (next)
import Data.Maybe



-- |The tuple of the OGER display system and OIS inputsystem
type BogreSystem = (DisplaySystem, InputSystem)

-- |An up and running boher system. This should only be used internally other than perhaps for accessing the frameE
data Frameworks t => HookedBogreSystem t = HookedBogreSystem {
        displaySystem  :: DisplaySystem,
        inputSystem    :: InputSystem,
        frameE         :: Event t BogreFrame,
        _updateWorldE  :: Event t BogreFrame  -- world is to be updated internally on this event
                                                        -- strictly for internal use!
                                                        -- This is used to allow Behaviours that are stepped events to be realized in the same frame
}

-- |Gets the keyboard press event for a HookedBogreSystem, Keys are polled at each frame
keysPressE :: Frameworks t => HookedBogreSystem t -> Event t KeysPressed
keysPressE bs = frameKeysPress <$> (frameE bs)

-- |Gets the mouse move event for a HookedBogreSystem. Mouse position is polled at each frame.
mouseMoveE :: Frameworks t => HookedBogreSystem t -> Event t MouseState
mouseMoveE bs = frameMouseMove <$> (frameE bs)

-- |All information captured in a single frame. This includes timing information and captured input information.
data BogreFrame = BogreFrame {
        -- |Start time of the frame.
        frameTi         :: Float,
        -- |End time of the frame.
        frameTf         :: Float,
        -- |Relative mouse position from the last frame
        frameMouseMove  :: MouseState,
        -- |List of key codes of currently pressed keyboard keys.
        frameKeysPress  :: KeysPressed
}
-- |Get the time delta of the frame. This should be thought of as the amount of time that the frame is displayed.
frameDt :: BogreFrame -> Float
frameDt f = (frameTf f) - (frameTi f)

-- |This is the same as frameTf
frameT :: BogreFrame -> Float
frameT = frameTf

-- |This is a dummy frame that can be used, for example, as an initial vlaue for a frame Behaviour. The start time
-- and end time are 0, and there is no input from the user
nullFrame :: BogreFrame
nullFrame = BogreFrame {
        frameTi = 0,
        frameTf = 0,
        frameMouseMove = (0,0),
        frameKeysPress = []
}

-- |All games should be described in a function of this type
type GameBuilder t = HookedBogreSystem t -> SceneManager -> Moment t ()

-- |Given a 'GameBuilder', this will setup the Boger system and run the game
runGame :: (forall t. Frameworks t => GameBuilder t) -> IO ()
runGame gameBuilder = do
        -- init the display system
        ds <- createDisplaySystem
        let smgr = sceneManager ds
        
        -- init input system
        handle <- getWindowHandler (window ds)
        is <- createInputSystem handle
        
        -- default camera
        cam <- sceneManager_getCamera smgr "PlayerCam"
        camera_setPosition_CameraPfloatfloatfloat cam 0 0 500
        camera_lookAt cam 0 0 (-300)
        
        -- default ambient light
        colourValue_with 0.5 0.5 0.5 1.0 (sceneManager_setAmbientLight smgr)
        
        -- create frame addhandler
        (frameAddHandler, frameFire) <- newAddHandler
        (updateWorldAddHandler, updateWorld) <- newAddHandler
        
        let
                frameworkNetwork bs = do
                        hookedBogreSystem <- hookBogerSystem bs frameAddHandler updateWorldAddHandler 
                        gameBuilder hookedBogreSystem smgr
                        
                        
        eventNet <- compile $ frameworkNetwork (ds, is)
        actuate eventNet
        
        -- A proper mechanism of handeling child threads is needed (withh respect to ending the main thread) 
        startBogreSync (ds, is) frameFire updateWorld   -- this will block the main thread untill the window is closed
        
-- |Call this function at the end the game to stop the Boger system. This can be done through reactimate as follows
--
-- 
-- >  reactimate $ (stopBogre bs) <$ someEvent@
-- 
stopBogre :: Frameworks t => HookedBogreSystem t -> IO ()
stopBogre bs = do
        -- TODO clean stop
        closeDisplaySystem $ displaySystem bs

-- |starts the Boger system and blocks untill 'stopBogre' is called
startBogreSync :: BogreSystem -> (BogreFrame -> IO ()) -> (BogreFrame -> IO ()) -> IO ()
startBogreSync (ds, is) frameFire updateWorld = do
        render win r () handler where
                win = window ds
                r = root ds
                handler _ ti tf _ = do
                        (ms, kp) <- capture is
                        let frame = BogreFrame{
                                frameTi = ti,
                                frameTf = tf,
                                frameMouseMove = ms,
                                frameKeysPress = kp
                        }
                        frameFire frame
                        updateWorld frame
                        return ((), True)

{-                
startBogre :: BogreSystem -> (BogreFrame -> IO ()) -> (BogreFrame -> IO ()) -> IO ()
startBogre bs frameFire updateWorld = do 
        _ <- forkIO $ startBogreSync bs frameFire updateWorld
        return ()
-}

-- |Will unhook a 'HookedBogerSystem' so that it is not tied to a Reactive-Banana context 't'
unhookBogerSystem :: Frameworks t => HookedBogreSystem t -> BogreSystem
unhookBogerSystem bs = (displaySystem bs, inputSystem bs)

-- | creates the basic events from the input system (can be done by hand, but using multiple input events causes Reactive-banana
-- to run into memory leaks (e.g. using 2 different frameE to reactimate the same behaviour causes a mem leak))
hookBogerSystem :: Frameworks t => BogreSystem -> AddHandler BogreFrame -> AddHandler BogreFrame -> Moment t (HookedBogreSystem t)
hookBogerSystem (ds,is) frameAddHandler updateWorldAddHandler = do
        fE <- fromAddHandler frameAddHandler
        uwE <- fromAddHandler updateWorldAddHandler
        return HookedBogreSystem {
              displaySystem = ds,
              inputSystem = is,
              frameE = fE,
              _updateWorldE = uwE
        }

-- |Given a scene node this will get the position as returned by the OGRE engine. Not that this is, conseptually, the nodes position
-- Behaviour if set elsewhere, but there is no guarantee that the behaviors will be equal at all times:
--
-- @
-- setPosB bs node posB1
-- posB2 <- getPositionB node
-- @
-- 
-- here @posB1@ and @posB2@ may have different values at any given time.
getPositionB :: Frameworks t => SceneNode -> Moment t (Behavior t Vec3)
getPositionB n = fromPoll $ getPosition n

-- |Get's the absolute position of the mouse. The position is not constrained to a window, so can grow indefinetly. The initial
-- mouse position is (0,0,0)
getMousePosB :: Frameworks t => HookedBogreSystem t ->  Behavior t Vec3
getMousePosB bs = velocityToPositionB bs (0,0,0) (getMouseVelB bs)

-- |Get's the velocity of the mouse. This is technically the average velocity of the mouse over the current frame
getMouseVelB :: Frameworks t => HookedBogreSystem t ->  Behavior t Vec3
getMouseVelB bs = stepper (0,0,0) (frameToVelocity <$> fE) where
        sensitivity = 0.5
        fE = frameE bs
        frameToVelocity f = scale (sensitivity / (frameDt f)) (mouseMoveToVec3 (frameMouseMove f))  
        mouseMoveToVec3 (x,y) = (fromIntegral x, negate (fromIntegral  y), 0)
        
{-
        DYNAMIC FUNCTIONS THAT HANDLE DYNAMICALLY CREATED NODES
        
        Due to not being able to dynamiclaly modify the EventNetwork with currentinput
        (e.g. REACTIMATE create new Behaviors and Event based on current Behs/Events)
        
        Implement everything in this dynamic way takign events with new nodes, and then
        allow a conversion to non-dynamic version that can simple take the arguments and
        artificially create an event for it that just fires once at time 0.     
-}

-- |Use this to dynamically create new nodes whenever the passed 'Event' occurs. The resulting 'Event' will contain the newly created node.
-- The value of the passed 'Event' is ignored.
createNodeOnE :: Frameworks t => HookedBogreSystem t -> Event t a -> Moment t (Event t SceneNode)
createNodeOnE bs createOnE = do
        let ubs = unhookBogerSystem bs
        let
                createNode :: Frameworks s =>  Moment s (SceneNode)
                createNode = do
                        (_,node) <- liftIO $ OGRE.addEntity (fst ubs) "ogrehead.mesh"
                        liftIO $ setPosition node (10000000,10000000,10000000)
                        return node
        execute (FrameworksMoment (createNode)  <$ createOnE)
        
        
-- |Set the position of a note to match a given 'Behavior t Vec3' at all times.
setPosB :: Frameworks t => HookedBogreSystem t -> SceneNode -> Behavior t Vec3  -> Moment t ()
setPosB bs node posB = do
        let dynamicB = ((:[]) . ((flip (,)) node)) <$> posB
        setDynamicPositionBs bs dynamicB

-- |Set the velocity of a note to match a given 'Behavior t Vec3' at all times. Not that the velocity 'Behavior' is only sampled
-- at the end of each frame. 
setVelB :: Frameworks t => HookedBogreSystem t -> SceneNode -> Behavior t Vec3  -> Moment t ()
setVelB bs node velB = do
        let dynamicB = ((:[]) . ((,) node)) <$> velB
        setDynamicVelBs bs dynamicB

-- |This will set the velocities of a variable number of nodes according to a 'Behavior' of a list of node-velocity pairs. Use this to
-- set the velocity of dynamically created nodes.
setDynamicVelBs :: Frameworks t => HookedBogreSystem t -> Behavior t [(SceneNode, Vec3)]  -> Moment t ()
setDynamicVelBs bs nodeVelB = do
        let uwE = _updateWorldE bs
        let sampleE = ((\nvs frame -> (frameDt frame, nvs)) <$> nodeVelB) <@> uwE
        let
                -- covert velocities to change in position
                toDPoses :: (Float, [(SceneNode, Vec3)]) -> [(SceneNode, Vec3)]
                toDPoses (dt, nodeVels) = map toDPos nodeVels where
                        toDPos (node, vel) = (node, scale dt vel) 
                
                -- move the nodes
                doUpdates :: [(SceneNode, Vec3)] -> IO ()
                doUpdates nodeDPoses = mapM_ doUpdate nodeDPoses where
                        doUpdate (node, dPos) = setPositionRelative node dPos
                
        reactimate $ (doUpdates . toDPoses) <$> sampleE

-- |This will set positions of a variable number of nodes according to a 'Behavior' of a list of node-position pairs. Use this to
-- set the position of dynamically created nodes. Note that this has a close relation to the output of 'getDynamicDelayedPosBs'
setDynamicPositionBs :: Frameworks t => HookedBogreSystem t -> Behavior t [(Vec3, SceneNode)]  -> Moment t ()
setDynamicPositionBs bs nodeVelB = do
        let uwE = _updateWorldE bs
        let sampleE = nodeVelB <@ uwE
        let
                -- move the nodes
                doUpdates :: [(Vec3, SceneNode)] -> IO ()
                doUpdates nodeDPoses = mapM_ doUpdate nodeDPoses where
                        doUpdate (pos, node) = setPosition node pos
                
        reactimate $ doUpdates <$> sampleE

-- | take a behaviour and dynamically create delays. Only the currently needed history is stored, so
-- if a large delay is added, the resulting value will just be the latest recorded value untill history catches up. The passed
-- Event should specify the delays. The output is a behaviour of corresponding delayed Vec3 (latest added delay is at the head of the list)
getDynamicDelayedPosBs :: Frameworks t =>  HookedBogreSystem t -> Behavior t Vec3 -> Event t  (Float, a) -> Moment t (Behavior t [(Vec3, a)])
getDynamicDelayedPosBs bs masterB delayTaggedE = getWithInitDynamicDelayedPositionBs bs masterB [] delayTaggedE

-- |Used internally to implement delayed position behaviors
type DynamicDelayStep a = (BogreFrame, [(Float,Vec3)], Float, [Float], [a], [Vec3])

-- |Used internally to implement delaye
getWithInitDynamicDelayedPositionBs :: Frameworks t =>  HookedBogreSystem t -> Behavior t Vec3 -> [(Float, a)] -> Event t (Float, a) -> Moment t (Behavior t [(Vec3, a)])
getWithInitDynamicDelayedPositionBs bs masterB initDelaysTaggeed delayTaggedE = do
        -- as it may take some time for the history to fill up before the delay can produce values, we need some default temporary value
        let defaultVal = (0,0,0)
        let initHist = [(0, defaultVal)]
        let
                -- whenever there is a new delay, dynamically add it to the list
                --      as it may take some time for the history to fill up before the delay can produce values,
                --      simply set it to defaultVal
                addDelay :: (Float, a) -> DynamicDelayStep a -> DynamicDelayStep a
                addDelay (newDelay, newTag) (frame, history, maxDelay, delays, tags, delayedVals) = next where
                        next = (frame, history, maxDelay', delays', tags', delayedVals')
                        maxDelay' = max maxDelay newDelay
                        delays' = newDelay : delays
                        tags' = newTag : tags
                        delayedVals' = defaultVal : delayedVals
                
                -- whenever there is a change to the master behaviour, add it to the history 
                --      we assume the behaviour changed at the start of the current frame
                --      This also prunes old events that are older than maxDelay
                addToHistory :: Vec3 -> DynamicDelayStep a -> DynamicDelayStep a
                addToHistory newVal (frame, history, maxDelay, delays, tags, delayedVals) = next where
                        next = (frame, history', maxDelay, delays, tags, delayedVals)
                        newValTime = frameTf frame
                        history' = (newValTime, newVal) : prune history where
                                -- prune old events
                                prune []                        = []
                                prune hist'@((a@(at,_)):xs) 
                                        | at >= newValTime - maxDelay   = a : prune xs
                                        -- save 1 extra element used in derive function --, and a final default value
                                        | otherwise                     = (head hist'):[] --initHist
                
                -- update the current frame
                updateFrame :: BogreFrame -> DynamicDelayStep a -> DynamicDelayStep a
                updateFrame frameNew (_, history, maxDelay, delays, tags, delayedVals) = next where
                        next = (frameNew, history, maxDelay, delays, tags, delayedVals)
                        
                -- whenever ready, progress the delayed behaviours to fit the current frame
                stepFrame ::  DynamicDelayStep a -> DynamicDelayStep a
                stepFrame (frame, history, maxDelay, delays, tags, _) = next where
                        next = (frame, history, maxDelay, delays, tags, delayedVals')
                        delayedVals' = map derive delays where
                                time = frameTf frame
                                histInc = reverse history        -- history in increasing time
                                dt = frameDt frame
                                derive delay = derivedVel where
                                        delayTime = time-delay
                                        derivedVel = derivedVelInteg
                                        derivedVelInteg | dt == 0       = defaultVal
                                                        | otherwise     = derive' histInc
                                                        
                                        derive' []                 = error("no previouse event???")
                                        derive' ((_,av):[])        = av
                                        derive' ((at,av):rest@((bt,bv):_))
                                                | bt <= delayTime   = derive' rest    -- move to first applicable velocity function
                                                -- once pruned, linearly interpolate the position
                                                | at <= delayTime   = (scale wa av) `add` (scale wb bv)
                                                | otherwise         = av where
                                                                        dtatb = bt - at
                                                                        wb = (delayTime - at) / dtatb
                                                                        wa = 1 - wb
                -- convert the output of these functions, to the actual delayed values
                getDelayedVals :: DynamicDelayStep a -> [(Vec3, a)]
                getDelayedVals (_,_,_,_,tags,dVals) = zip dVals tags

        -- frame event
        let fE = frameE bs
        -- changes to the master behavior Event
        masterChangeE <- changes masterB
        -- dynamically add a delay Event
        -- delayE
        let initProps = foldl (flip addDelay) (nullFrame, initHist, 0, [], [], []) initDelaysTaggeed
        let stepsB = (accumB initProps (
                        (addDelay       <$>  delayTaggedE) `union`
                        (updateFrame    <$>  fE) `union` 
                        (addToHistory   <$>  masterChangeE) `union` -- use previouse time as that is when the behaviour started (at the start of this frame)
                        (stepFrame      <$   masterChangeE)
                ))
        let delayedB = getDelayedVals <$> stepsB
        return delayedB


-- |Converts a velocity to position 'Behavior'. Note that the velocity is simple sampled at the end of each frame, so if the velocity
-- changes many times in a frame, or was not valid for the duration of that frame, then the resulting posiiton may be inacurate.
velocityToPositionB :: Frameworks t => HookedBogreSystem t -> Vec3 -> Behavior t Vec3 -> Behavior t Vec3
velocityToPositionB bs initPos vel = accumB initPos (add <$> dPosE) where
        dPosE = (((flip scale) <$> vel) <@> (frameDt <$> (frameE bs)))

-- |Time delay a position 'Behavior'
getDelayedPosB :: Frameworks t =>  HookedBogreSystem t -> Behavior t Vec3 -> Float -> Moment t (Behavior t Vec3)
getDelayedPosB bs velB delay = do
        dynVelBs <- getWithInitDynamicDelayedPositionBs bs velB [(delay, ())] never
        return $ (fst . head) <$> dynVelBs where

{-     NOT USED AS VELOCITIES ARE NOT PROPERLLY INTERPOLATED OVER FRAMES
-- |Time delay a velocity 'Behavior'
getDelayedPosB :: Frameworks t =>  HookedBogreSystem t -> Behavior t Vec3 -> Float -> Moment t (Behavior t Vec3)
getDelayedPosB bs velB delay = do
        dynVelBs <- getWithInitDynamicDelayedPositionBs bs velB [(delay, ())] never
        return $ (fst . head) <$> dynVelBs where
-}
        
-- |The current frame time (see 'framT'). Not that as this is a stepped 'Behavior', it only changes after the frame event occurs,
-- so if this is sampled on the frame event, it wall only be the previouse frame's time. 
getTimeB :: Frameworks t => HookedBogreSystem t -> Behavior t Float
getTimeB bs = stepper 0 (frameT <$> (frameE bs))


-- |Get the KeyState changes (Up and Down) Event for a single key. Note that only the changes are visible, so the 
-- events will always alternate be Up and Down (i.e. there will not be 2 Down events or 2 Up events in sequence) 
getKeyStateE :: Frameworks t => HookedBogreSystem t -> KeyCode -> Event t KeyState
getKeyStateE bs key = removeDuplicates myKeyStatesE where
        allKeyPressE = keysPressE bs
        -- convert to Mouse state (now we have runs of Ups and Downs)
        myKeyStatesE = toMouseState <$> allKeyPressE  where
                toMouseState keysDown | elem key keysDown       = Down
                                      | otherwise               = Up

-- |Get the key down event, for a given key, that occurs when a key is pushed down.
getKeyDownE :: Frameworks t => HookedBogreSystem t -> KeyCode -> Event t KeyState
getKeyDownE bs key = filterE (== Down) (getKeyStateE bs key)
                
-- |Get the key up event, for a given key, that occurs when a key is released.
getKeyUpE :: Frameworks t => HookedBogreSystem t -> KeyCode -> Event t KeyState
getKeyUpE bs key = filterE (== Up) (getKeyStateE bs key)

-- |Adds a mesh to the world, given the mesh's file name.
addEntity :: Frameworks t => HookedBogreSystem t -> String -> IO (SceneNode)
addEntity bs meshFileName = fmap snd (OGRE.addEntity (displaySystem bs) meshFileName)

-- |Gets a 'Behavior' or random values. This can be called multiple times to get multiple different random 'Behavior's:
--
-- @
-- r1B <- getRandomB
-- r2B <- getRandomB
-- @
--
-- In this case @r1B@ and @r2B@ will be 2 seperatly generated randome values.
-- Note that values will be generated according to how 'a' is defined as an instance of the 'Random' class.
getRandomB :: (Frameworks t, Random a) => Moment t (Behavior t a)
getRandomB = fromPoll randomIO

-- |Gets a 'Behavior' or random 'Vec3' values. This can be called multiple times to get multiple different random 'Behavior's.
-- The value of each dimention is generated independantly to be a value between 0 and 1.
getRandomVec3B :: Frameworks t => Moment t (Behavior t Vec3)
getRandomVec3B = do
        xB <- getRandomB
        yB <- getRandomB
        zB <- getRandomB
        let xyzB = (\x y z -> (x,y,z)) <$> xB <*> yB <*> zB
        return xyzB
    
-- |Checks for collisions at each frame and fires an event when they colide. The 2 position behaviours must move appart before
-- a second event is fired (if the objects colide and stay colidded, only one event will be fired).   
sphereCollisionsE :: Frameworks t => HookedBogreSystem t -> Float -> Behavior t Vec3 -> Behavior t Vec3  -> Event t ()
sphereCollisionsE bs radius posAB posBB = collisionE where
        collisionE = () <$ (filterE (==True) (removeDuplicates (isCollidedB <@ fE)))
        fE = frameE bs
        isCollidedB = (<= sqrRadius) <$> (sqrDistB)
        sqrDistB = sqrDist <$> posAB <*> posBB
        sqrRadius = radius**2

-- |Filters an 'Event' such that the save event only occurse once. i.e. events [1,1,1,2,2,2,1,1,1,4,5,5,4,5,6] would become [1,2,1,4,5,4,5,6]
removeDuplicates :: (Frameworks t, Eq a) => Event t a -> Event t a
removeDuplicates e = dubE where
        dubE = (fromJust . fst) <$> (filterE (uncurry (/=)) prevZip)
        prevZip = accumE (Nothing,Nothing) ((\curr (prev,_) -> (Just curr, prev)) <$> e)






        
