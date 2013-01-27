module BB.Examples.HogreHois05
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import OIS.Types

import Reactive.Banana.Combinators
import Reactive.Banana.Switch

import Reactive.Banana.OGRE
import Reactive.Banana.OIS
import Reactive.Banana.BOGRE

import BB.Workarounds
import BB.Util.Vec

import Data.List (insert, find)
import Data.Maybe

import Control.Concurrent (threadDelay)

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
hogreHois05 :: IO ()
hogreHois05 = do
        ds <- createDisplaySystem
        
        let smgr = sceneManager ds
        cam <- sceneManager_getCamera smgr "PlayerCam"
        camera_setPosition_CameraPfloatfloatfloat cam 0 0 200
        camera_lookAt cam 0 0 (-300)
        
        --load oger heads
        (_, headNode1) <- addEntity ds "ogrehead.mesh"
        (_, headNode2) <- addEntity ds "ogrehead.mesh"
        
        -- set ambient light
        colourValue_with 0.5 0.5 0.5 1.0 (sceneManager_setAmbientLight smgr)

        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 20 80 50
        
        -- create input system
        handle <- getWindowHandler (window ds)
        is <- createInputSystem handle
        
        eventNet <- compile $ network ds is (headNode1, headNode2)
        actuate eventNet
        
        
        -- A proper mechanism of handeling child threads is needed (withh respect to ending the main thread) 
        
        --startRendering ds (capture is)    -- this will not blockk the main thread, hence main will exit right away
        startBogreSync (ds, is)   -- this will block the main thread untill the window is closed
        
        closeDisplaySystem ds

network :: Frameworks t => DisplaySystem -> InputSystem -> (SceneNode,SceneNode) -> Moment t ()
network ds is (node1,node2) = do
        -- get Key input Event stream
        keyE <- getKeyE is
        
        -- Mouse input (as a velocity Behaviour)
        mouseVelB <- getMouseVelocityB is
        
        -- set node1 velocity to mouse velocity
        setVelocityB ds node1 mouseVelB
        
        deVelB <- getDelayedVelocityB ds node2 mouseVelB 4
        setVelocityB ds node2 deVelB
        
        -- set node2 velocity to mouse velocity with a time delay
        --setDelayedVelocityB ds node2 mouseVelB 0
        
        -- stop on escape key
        reactimate $ (closeDisplaySystem ds) <$ (filterE (elem KC_ESCAPE) keyE)

getMouseVelocityB :: Frameworks t => InputSystem ->  Moment t (Behavior t Vec3)
getMouseVelocityB is = do
        mouseE <- getMouseE is
        let vel = stepper (0,0,0) (mouseMoveToVelocity <$> mouseE) where
                sensitivity = 20
                mouseMoveToVelocity (x,y) = scale sensitivity (fromIntegral x, negate (fromIntegral  y), 0)
        return vel
    
setVelocityB :: Frameworks t => DisplaySystem -> SceneNode -> Behavior t Vec3  -> Moment t ()
setVelocityB ds node velocityB = do
        dtE <- getFrameEvent ds
        let posE = accumE (0,0,0) (add <$> (((flip scale) <$> velocityB) <@> dtE))
        reactimate $ (setPosition node) <$> posE
        


setDelayedVelocityB :: Frameworks t =>  DisplaySystem -> SceneNode -> Behavior t Vec3 -> Float -> Moment t ()
setDelayedVelocityB ds node velocityB delay = do
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
        
        let delVelB = stepper (\x -> scale x initVel) $ (derive <$> tB) <@> velHistoryAbsTE
        
        -- use velocity behaviors to accumulate positions
        let pos2E = accumE (0,0,0) (add <$> ((delVelB) <@> dtE))
        reactimate $ (setPosition node) <$> pos2E

    
getDelayedVelocityB :: Frameworks t =>  DisplaySystem -> SceneNode -> Behavior t Vec3 -> Float -> Moment t (Behavior t Vec3)
getDelayedVelocityB ds node velocityB delay = do
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
        --return $ delVelB <*> (pure 1)
        {-
        dtE <- getFrameEvent ds
        let posE = accumE (0,0,0) (add <$> (((flip scale) <$> velocityB) <@> dtE))
        reactimate $ (setPosition node) <$> posE
        -- use velocity behaviors to accumulate positions
        let pos2E = accumE (0,0,0) (add <$> ((delVelB) <@> dtE))
        reactimate $ (setPosition node) <$> pos2E
        -}
              
printKey :: Show a => a -> IO ()
printKey pressedKeys = do
        putStrLn $ "Pressed keys: " ++ (show pressedKeys)
        
        
        
        
        
        
        
