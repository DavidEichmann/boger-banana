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

import BB.Util.Vec


-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials

snake :: IO ()
snake = runGame myGame

-- init the world and return the FRP network
initWorld :: SceneManager -> IO ()
initWorld smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 20 80 50


myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- init the world
        liftIO $ initWorld smgr
        
        -- mouse input
        posB <- getMousePosB bs

        -- initial head
        (_,node0) <- liftIO $ addEntity (displaySystem bs) "ogrehead.mesh"
        setPosB bs node0 posB
        
        -- dynamically add heads
        spaceKeyE <- getKeyDownE bs KC_SPACE
        newHeadE <- createNodeOnE bs spaceKeyE
        -- delay heads
        let lastHead = stepper node0 newHeadE
        let headCountB = accumB 1 ((+1) <$ newHeadE)
        let headsDelaysE = (((,) . (*0.2))  <$> headCountB) <@> newHeadE
        delayedB <- getDynamicDelayedPositionBs bs posB headsDelaysE
        setDynamicPositions bs delayedB
        
        -- stop on escape key
        escE <- getKeyDownE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE
        return ()

        
        
        
