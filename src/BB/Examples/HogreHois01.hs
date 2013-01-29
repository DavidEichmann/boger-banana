module BB.Examples.HogreHois01
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import OIS.Types

import Reactive.Banana.Combinators

import Reactive.Banana.OGRE
import Reactive.Banana.OIS
import Reactive.Banana.BOGRE

import BB.Workarounds
import BB.Util.Vec

import Data.List (insert)

import Control.Concurrent (threadDelay)

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
hogreHois01 :: IO ()
hogreHois01 = do
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
        keyE <- getKeysE is
        frameE <- getFrameEvent ds

        -- network
        
        -- time
        -- at each frame
        let absTimeFrameEvents = accumE 0 ((+) <$> frameE)
        
        let now  = absTimeFrameEvents
        let past = apply (pure (\t -> t-2)) absTimeFrameEvents
        
        -- position of node as a function of time
        let posBehavior  = pure  (\t -> scale 40 (sin t, 0, cos t))
        
        let pos1 = apply posBehavior now
        let pos2 = apply posBehavior past
        
        -- output
        reactimate $ (setPosition node1) <$> pos1  
        reactimate $ (setPosition node2) <$> pos2
        
        -- print keydowns
        reactimate $ printKey <$> keyE
        
        -- stop on escape key
        reactimate $ (closeDisplaySystem ds) <$ (filterE (elem KC_ESCAPE) keyE) 
        
         
printKey :: Show a => a -> IO ()
printKey pressedKeys = do
        putStrLn $ "Pressed keys: " ++ (show pressedKeys)
        
        
        
        
        
        
        
        