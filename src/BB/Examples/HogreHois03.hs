module BB.Examples.HogreHois03
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
hogreHois03 :: IO ()
hogreHois03 = do
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
        keyE <- getKeyE is
        mouseE <- getMouseE is
        dtE <- getFrameEvent ds  -- think of as time delta (dt)
        let tE = accumE 0 ((+) <$> dtE)      -- think of this as absolute time
        let tB = stepper 0 tE                  -- think of this as absolute time

        -- network
        
        -- time
        -- at each frame
        
        -- Movement behaviors all movements at 1 per second
        let nullFn      = (\_ -> (0,0,0))
        let leftFn      = (\dt -> scale dt (-60,0,0))
        let rightFn     = (\dt -> scale dt (60,0,0))
        let upFn        = (\dt -> scale dt (0,60,0))
        let downFn      = (\dt -> scale dt (0,-60,0))
        
        let mouseMoveE = (\(x,y) -> (fromIntegral x, negate (fromIntegral  y), 0)) <$> mouseE
                              
        let pos1B = accumB (0,0,0) (fmap (add . (scale (0.25))) mouseMoveE)
        --let pd1E = apply (movementB) dtE
        --let pos1E = accumE (0,0,0) (apply (pure add) pd1E)
        
        --follow
        let pos2B = accumB (0,0,0) (fmap add (apply (
                        (fmap (\v dt -> scale (dt/2) v) ((fmap to pos2B) <*> pos1B))
                ) dtE))
        
        pos1E <- changes pos1B
        pos2E <- changes pos2B
        
        -- output
        reactimate $ (setPosition node1) <$> pos1E
        reactimate $ (setPosition node2) <$> pos2E
        --reactimate $ (putStrLn. show) <$> mouseE
        -- stop on escape key
        reactimate $ (closeDisplaySystem ds) <$ (filterE (elem KC_ESCAPE) keyE) 
        
         
printKey :: Show a => a -> IO ()
printKey pressedKeys = do
        putStrLn $ "Pressed keys: " ++ (show pressedKeys)
        
        
        
        
        
        
        
        