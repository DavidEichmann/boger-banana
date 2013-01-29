module BB.Examples.Snake
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

snake :: IO ()
snake = runGame gameBuilder

-- init the world and return the FRP network
gameBuilder :: GameBuilder
gameBuilder bs@(ds,_) smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 20 80 50
        
        -- TODO: init world
        
        -- create FRP network
        compile (network bs ???)


network :: Frameworks t => BogreSystem -> ???world??? -> Moment t ()
network bs (node1,node2) = do
        -- TODO Snake
        {-
                create an IO (node) function that creates new body parts, returning the node.
                this IO function gets called whenever the body colides with the target
                        map the IO function to a collision Event via the "execute" function.
                                this results in a new Event t node that will have an event contianing the node
                                whenever a new bodypart is created.
                                
                Some how need to create the behaviour for the dynamically created nodes (dynamic Event/Behaviour switching)
        -}
        
        -- stop on escape key
        escE <- getKeyE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE

        
        
        
