module BB.Examples.HogreHois05
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import OIS.Types


import Reactive.Banana.OGRE
import Reactive.Banana.OIS
import Reactive.Banana.BOGRE


-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials

hogreHois05 :: IO ()
hogreHois05 = runGame gameBuilder

-- init the world and return the FRP network
gameBuilder :: GameBuilder
gameBuilder bs@(ds,_) smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 20 80 50
        
        -- load oger heads
        (_, headNode1) <- addEntity ds "ogrehead.mesh"
        (_, headNode2) <- addEntity ds "ogrehead.mesh"
        
        -- create FRP network
        compile (network bs (headNode1, headNode2))


network :: Frameworks t => BogreSystem -> (SceneNode,SceneNode) -> Moment t ()
network bs (node1,node2) = do
        -- Mouse input (as a velocity Behaviour)
        mouseVelB <- getMouseVelocityB bs
        
        -- set node1 velocity to mouse velocity
        setVelocityB bs node1 mouseVelB
        
        deVelB <- getDelayedVelocityB bs mouseVelB 4
        setVelocityB bs node2 deVelB
        
        -- stop on escape key
        escE <- getKeyE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE

        
        
        
