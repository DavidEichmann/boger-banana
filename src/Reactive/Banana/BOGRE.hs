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

type BogreSystem = (DisplaySystem, InputSystem)
type GameBuilder = BogreSystem -> SceneManager -> IO (EventNetwork)

runGame :: GameBuilder -> IO ()
runGame gameBuilder = do
        -- init the display system
        ds <- createDisplaySystem
        let smgr = sceneManager ds
        
        -- init input system
        handle <- getWindowHandler (window ds)
        is <- createInputSystem handle
        
        -- default camera
        cam <- sceneManager_getCamera smgr "PlayerCam"
        camera_setPosition_CameraPfloatfloatfloat cam 0 0 200
        camera_lookAt cam 0 0 (-300)
        
        -- default ambient light
        colourValue_with 0.5 0.5 0.5 1.0 (sceneManager_setAmbientLight smgr)
        
        eventNet <- gameBuilder (ds, is) smgr
        actuate eventNet
        
        -- A proper mechanism of handeling child threads is needed (withh respect to ending the main thread) 
        startBogreSync (ds, is)   -- this will block the main thread untill the window is closed

stopBogre :: BogreSystem -> IO ()
stopBogre (ds, _) = do
        -- TODO clean stop
        closeDisplaySystem ds

startBogreSync :: BogreSystem -> IO ()
startBogreSync (ds, is) = render win r () handler
                where
                        win = window ds
                        r = root ds
                        handler _ td _ = do
                                capture is
                                fireFrameEvent ds td
                                return ((), True)
                
startBogre :: BogreSystem -> IO ()
startBogre bs = do 
        _ <- forkIO $ startBogreSync bs
        return ()

getMouseVelocityB :: Frameworks t => BogreSystem ->  Moment t (Behavior t Vec3)
getMouseVelocityB (_,is) = do
        mouseE <- getMouseE is
        let vel = stepper (0,0,0) (mouseMoveToVelocity <$> mouseE) where
                sensitivity = 20
                mouseMoveToVelocity (x,y) = scale sensitivity (fromIntegral x, negate (fromIntegral  y), 0)
        return vel
    
setVelocityB :: Frameworks t => BogreSystem -> SceneNode -> Behavior t Vec3  -> Moment t ()
setVelocityB (ds,_) node velocityB = do
        dtE <- getFrameEvent ds
        let posE = accumE (0,0,0) (add <$> (((flip scale) <$> velocityB) <@> dtE))
        reactimate $ (setPosition node) <$> posE
    
getDelayedVelocityB :: Frameworks t =>  BogreSystem -> Behavior t Vec3 -> Float -> Moment t (Behavior t Vec3)
getDelayedVelocityB (ds,_) velocityB delay = do
        dtE <- getFrameEvent ds
        let tE = accumE 0 ((+) <$> dtE)      -- think of this as absolute time
        let tB = stepper 0 tE                  -- think of this as absolute time
        velChangeE <- changes velocityB
        initVel <- initial velocityB
        let initHist = [(0,initVel)]
        let velHistoryAbsTE = accumE initHist (pushDrop <$> velChangeTaggedE) where
                velChangeTaggedE = apply ((,) <$> tB) velChangeE
                pushDrop e hist = e : prune hist where
                        time = fst e
                        -- prune old events
                        prune []                        = []
                        prune hist'@((a@(at,_)):xs) 
                                | at >= time-delay      = a : prune xs
                                | otherwise             = take 1 hist' -- save 1 extra element used in derive function
                        
        -- The time delta dt is  normally applied to a single velocity function, but the dt may need to be
        -- applied to many different velocity functions if dt is large enough such that the velocity behavior
        -- changed delay time ago within that time delta
        -- split dt over any valid functions
        let derive time histDec dt = derivedVel where
                derivedVel      | dt == 0       = (0,0,0)
                                | otherwise     = derive' histInc dt (time-delay)
                histInc = reverse histDec        -- in increasing time
                -- think of the arguments as:
                        -- history (increasing) left to look at
                        -- time left
                        -- current time going through the history
                derive' [] dt' _                 = error("no previouse event???")
                derive' ((_,av):[]) dt' _        = scale dt' av
                derive' ((_,av):rest@((bt,_):_)) dt' time'
                        | bt <= time'             = derive' rest dt' time'    -- move to first applicable velocity function
                        -- once pruned, apply the velocity, handling the first special case where the time is inbetween events
                        | otherwise              = (scale timeOnA av) `add` (derive' rest dt'' bt) where
                                                        dt'' = dt' - timeOnA
                                                        timeOnA = min (bt-time') (dt')
        
        let delVelB = stepper (\dt -> (scale dt initVel)) ((derive <$> tB) <@> velHistoryAbsTE)
        return $ stepper (0,0,0) (((\velFn dt -> scale (1/dt) (velFn dt)) <$> delVelB) <@> dtE)


getKeyE :: Frameworks t => BogreSystem -> KeyCode -> Moment t (Event t KeysPressed)
getKeyE (_,is) key = do
        keyE <-  getKeysE is
        return $ filterE (elem key) keyE