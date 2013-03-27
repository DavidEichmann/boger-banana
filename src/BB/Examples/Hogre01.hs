module BB.Examples.Hogre01
where

import Reactive.Banana

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types

import Reactive.Banana.Frameworks
import Reactive.Banana.BOGRE.OGRE

import BB.Util.Vec

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
hogre01 :: IO ()
hogre01 = do
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
        
        
        eventNet <- compile $ network ds (headNode1, headNode2)
        actuate eventNet
        
        -- A proper mechanism of handeling child threads is needed (withh respect to ending the main thread) 
        
        --startRendering ds     -- this will not blockk the main thread, hence main will exit right away
        startRenderingSync ds   -- this will block the main thread untill the window is closed
        
        closeDisplaySystem ds
        


network :: Frameworks t => DisplaySystem -> (SceneNode,SceneNode) -> Moment t ()
network ds (node1,node2) = do
        -- input
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
        
        
        
        
        
        
        
        