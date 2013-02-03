module BB.Examples.HogreHois04
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

import Data.List (insert)
import Data.Maybe

import Control.Concurrent (threadDelay)

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
hogreHois04 :: IO ()
hogreHois04 = do
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
        -- input
        keyE <- getKeyDownE (ds, is) KC_ESCAPE
        mouseE <- getMouseE is
        dtE <- getFrameEvent ds  -- think of as time delta (dt)
        let tE = accumE 0 ((+) <$> dtE)      -- think of this as absolute time
        let tB = stepper 0 tE                  -- think of this as absolute time

        -- network
        
        -- time
        -- at each frame
        
        -- Movement behaviors all movements at 1 per second
        let mouseMoveE = (\(x,y) -> (fromIntegral x, negate (fromIntegral  y), 0)) <$> mouseE
                              
        let pos1B = accumB (0,0,0) (fmap (add . (scale (0.25))) mouseMoveE)
        --let pd1E = apply (movementB) dtE
        --let pos1E = accumE (0,0,0) (apply (pure add) pd1E)
        
        --follow
        let delay = 3
        let historyMouseE = accumE [(0,(0,0))] (pushDrop <$> taggedMouseE) where
                taggedMouseE = apply ((,) <$> tB) mouseE
                pushDrop e hist = e : (dropped hist) where
                        time = fst e
                        dropped = takeWhile ((> time-delay) . fst)      -- prune old events
        
        --reactimate $ (putStrLn "lol") <$ (filterE (\(a,b) -> a /= 0 && b /= 0) mouseE)
        let delayedMouseB = stepper [] historyMouseE 
        let delayedMouseE = (histLookup <$> delayedMouseB) <@> tE where
                histLookup hist time = (snd.headOrNull) (dropWhile ((> time-delay) . fst) hist)
                headOrNull (head:_) = head
                headOrNull _ = (0,(0,0))
        reactimate $ (putStrLn . show) <$> delayedMouseE
        let delayedMME = (\(x,y) -> (fromIntegral x, negate (fromIntegral  y), 0)) <$> delayedMouseE
        let pos2B = accumB (0,0,0) (fmap (add . (scale (0.25))) delayedMME)
        
        {-
        --
        --
                Delayed events happen according to mouse events... they are correct, due to variable frame rates,
                there are nocticable errors. Must think of an errorfree way of avoiding these errors. possibly by
                abstracting away the time events, and using only behaviourse that are functions of time (or dt)....
                delay behaviours instead of events
        --
        --
        -}
        
        pos1E <- changes pos1B
        pos2E <- changes pos2B
        
        -- output
        reactimate $ (setPosition node1) <$> pos1E
        reactimate $ (setPosition node2) <$> pos2E
        -- stop on escape key
        reactimate $ (closeDisplaySystem ds) <$ keyE 
        
         
printKey :: Show a => a -> IO ()
printKey pressedKeys = do
        putStrLn $ "Pressed keys: " ++ (show pressedKeys)
        
        
        
        
        
        
        
        
