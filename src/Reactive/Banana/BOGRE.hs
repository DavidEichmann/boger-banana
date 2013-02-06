{-
This is a fusion of the OIS and OGRE modules
-}
module Reactive.Banana.BOGRE where



import Control.Concurrent (forkIO)

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.OIS
import Reactive.Banana.OGRE

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import BB.Workarounds

import BB.Util.Vec



-- types

type BogreSystem = (DisplaySystem, InputSystem)

data Frameworks t => HookedBogreSystem t = HookedBogreSystem {
        displaySystem  :: DisplaySystem,
        inputSystem    :: InputSystem,
        frameE         :: Event t BogreFrame,
        _captureInputE :: Event t BogreFrame, -- get input in this event so that stepped input behaviours are used in the current frame, not the next
        _updateWorldE  :: Event t BogreFrame  -- world is to be updated internally on this event
                                                        -- strictly for internal use!
                                                        -- This is used to allow Behaviours that are stepped events to be realized in the same frame
}
keysPressE bs = frameKeysPress <$> (frameE bs)
mouseMoveE bs = frameMouseMove <$> (frameE bs)

data BogreFrame = BogreFrame {
        frameTi         :: Float, -- start time of the frame
        frameTf         :: Float, -- end time of the frame
        frameMouseMove  :: MouseState,
        frameKeysPress  :: KeysPressed
}
frameDt f = (frameTf f) - (frameTi f)
frameT = frameTf
nullFrame = BogreFrame {
        frameTi = 0,
        frameTf = 0,
        frameMouseMove = (0,0),
        frameKeysPress = []
}

type GameBuilder t = HookedBogreSystem t -> SceneManager -> Moment t ()


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
                frameworkNetwork gameBuilder bs frameAddHandler updateWorldAddHandler smgr = do
                        hookedBogreSystem <- hookBogerSystem bs frameAddHandler updateWorldAddHandler 
                        gameBuilder hookedBogreSystem smgr
                        
                        
        eventNet <- compile $ frameworkNetwork gameBuilder (ds, is) frameAddHandler updateWorldAddHandler smgr
        actuate eventNet
        
        -- A proper mechanism of handeling child threads is needed (withh respect to ending the main thread) 
        startBogreSync (ds, is) frameFire updateWorld   -- this will block the main thread untill the window is closed
        

stopBogre :: Frameworks t => HookedBogreSystem t -> IO ()
stopBogre bs = do
        -- TODO clean stop
        closeDisplaySystem $ displaySystem bs

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
                
startBogre :: BogreSystem -> (BogreFrame -> IO ()) -> (BogreFrame -> IO ()) -> IO ()
startBogre bs frameFire updateWorld = do 
        _ <- forkIO $ startBogreSync bs frameFire updateWorld
        return ()

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

getPositionB :: Frameworks t => SceneNode -> Moment t (Behavior t Vec3)
getPositionB n = fromPoll $ getPosition n

getMousePosB :: Frameworks t => HookedBogreSystem t ->  Moment t (Behavior t Vec3)
getMousePosB bs = do
        velB <- getMouseVelocityB bs
        return $ velocityToPositionB bs (0,0,0) velB

getMouseVelocityB :: Frameworks t => HookedBogreSystem t ->  Moment t (Behavior t Vec3)
getMouseVelocityB bs = do
        let mouseE = mouseMoveE bs
        let vel = stepper (0,0,0) (mouseMoveToVelocity <$> mouseE) where
                sensitivity = 20
                mouseMoveToVelocity (x,y) = scale sensitivity (fromIntegral x, negate (fromIntegral  y), 0)
        return vel
        
{-
        DYNAMIC FUNCTIONS THAT HANDLE DYNAMICALLY CREATED NODES
        
        Due to not being able to dynamiclaly modify the EventNetwork with currentinput
        (e.g. REACTIMATE create new Behaviors and Event based on current Behs/Events)
        
        Implement everything in this dynamic way takign events with new nodes, and then
        allow a conversion to non-dynamic version that can simple take the arguments and
        artificially create an event for it that just fires once at time 0.     
-}

-- Whenever an event fires, a new node will be created and returned in a new event stream
createNodeOnE :: Frameworks t => HookedBogreSystem t -> Event t a -> Moment t (Event t SceneNode)
createNodeOnE bs createOnE = do
        let ubs = unhookBogerSystem bs
        let
                createNode :: Frameworks s => BogreSystem -> Moment s (SceneNode)
                createNode ubs = do
                        (_,node) <- liftIO $ addEntity (fst ubs) "ogrehead.mesh"
                        liftIO $ setPosition node (10000000,10000000,10000000)
                        return node
        execute (FrameworksMoment (createNode ubs)  <$ createOnE)
        
        
    
setPosB :: Frameworks t => HookedBogreSystem t -> SceneNode -> Behavior t Vec3  -> Moment t ()
setPosB bs node posB = do
        let dynamicB = ((:[]) . ((flip (,)) node)) <$> posB
        setDynamicPositions bs dynamicB
    
setVelocityB :: Frameworks t => HookedBogreSystem t -> SceneNode -> Behavior t Vec3  -> Moment t ()
setVelocityB bs node velocityB = do
        let dynamicB = ((:[]) . ((,) node)) <$> velocityB
        setDynamicVelocities bs dynamicB

setDynamicVelocities :: Frameworks t => HookedBogreSystem t -> Behavior t [(SceneNode, Vec3)]  -> Moment t ()
setDynamicVelocities bs nodeVelocityB = do
        let uwE = _updateWorldE bs
        let sampleE = ((\nvs frame -> (frameDt frame, nvs)) <$> nodeVelocityB) <@> uwE
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

setDynamicPositions :: Frameworks t => HookedBogreSystem t -> Behavior t [(Vec3, SceneNode)]  -> Moment t ()
setDynamicPositions bs nodeVelocityB = do
        let uwE = _updateWorldE bs
        let sampleE = nodeVelocityB <@ uwE
        let
                -- move the nodes
                doUpdates :: [(Vec3, SceneNode)] -> IO ()
                doUpdates nodeDPoses = mapM_ doUpdate nodeDPoses where
                        doUpdate (pos, node) = setPosition node pos
                
        reactimate $ doUpdates <$> sampleE

-- | take a behaviour and dynamically create delays. Only the currently needed history is stored, so
-- if a large delay is added, the resulting value will just be the latest recorded value untill history catches up
-- event takes delays
-- output is a behaviour of corresponding delayed Vec3 (latest added is at the head of the list)
getDynamicDelayedPositionBs :: Frameworks t =>  HookedBogreSystem t -> Behavior t Vec3 -> Event t  (Float, a) -> Moment t (Behavior t [(Vec3, a)])
getDynamicDelayedPositionBs bs masterB delayTaggedE = getWithInitDynamicDelayedPositionBs bs masterB [] delayTaggedE

type DynamicDelayStep a = (BogreFrame, [(Float,Vec3)], Float, [Float], [a], [Vec3])

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
                updateFrame frameNew (frame, history, maxDelay, delays, tags, delayedVals) = next where
                        next = (frameNew, history, maxDelay, delays, tags, delayedVals)
                        
                -- whenever ready, progress the delayed behaviours to fit the current frame
                stepFrame ::  DynamicDelayStep a -> DynamicDelayStep a
                stepFrame (frame, history, maxDelay, delays, tags, delayedVals) = next where
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



{-
the non-dynamic version may look like

setNotDynamicDelayedEvents bs beh delay = setDynamicDelayedEvents bs beh (delay <$ networkStartE)

-}

velocityToPositionB :: Frameworks t => HookedBogreSystem t -> Vec3 -> Behavior t Vec3 -> Behavior t Vec3
velocityToPositionB bs initPos vel = accumB initPos (add <$> dPosE) where
        dPosE = (((flip scale) <$> vel) <@> (frameDt <$> (frameE bs)))

getDelayedB :: Frameworks t =>  HookedBogreSystem t -> Behavior t Vec3 -> Float -> Moment t (Behavior t Vec3)
getDelayedB bs velocityB delay = do
        dynVelBs <- getWithInitDynamicDelayedPositionBs bs velocityB [(delay, ())] never
        return $ (fst . head) <$> dynVelBs where


-- | get the KeyState changes (Up and Down) Event for a single key. 
getKeyStateE ::Frameworks t => HookedBogreSystem t -> KeyCode -> Moment t (Event t KeyState)
getKeyStateE bs key = do
        let allKeyPressE = keysPressE bs
        -- convert to Mouse state (now we have runs of Ups and Downs)
        let myKeyStatesE = toMouseState <$> allKeyPressE  where
                toMouseState keysDown | elem key keysDown       = Down
                                      | otherwise               = Up
                                      
        let zipedMyKeyStatesE = accumE (Up,Up) ((\curr (prev,_) -> (curr, prev)) <$> myKeyStatesE)
        let myKeyStateChangesE = fst <$> (filterE (uncurry (/=)) zipedMyKeyStatesE)
        return myKeyStateChangesE
        

getKeyDownE :: Frameworks t => HookedBogreSystem t -> KeyCode -> Moment t (Event t KeyState)
getKeyDownE bs key = do
        ksE <- getKeyStateE bs key
        return $ filterE (== Down) ksE
                
getKeyUpE :: Frameworks t => HookedBogreSystem t -> KeyCode -> Moment t (Event t KeyState)
getKeyUpE bs key = do
        ksE <- getKeyStateE bs key
        return $ filterE (== Up) ksE
        

        