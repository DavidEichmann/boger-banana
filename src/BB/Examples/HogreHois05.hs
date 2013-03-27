module BB.Examples.HogreHois05
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import OIS.Types

import Reactive.Banana.BOGRE.OGRE hiding (addEntity)
import Reactive.Banana.BOGRE.OIS
import Reactive.Banana.BOGRE

import BB.Util.Vec


-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials

hogreHois05 :: IO ()
hogreHois05 = runGame myGame

-- init the world and return an ogre head node
initWorld :: Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode)
initWorld bs smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 0 0 500
        
        -- default camera
        cam <- sceneManager_getCamera smgr "PlayerCam"
        camera_setPosition_CameraPfloatfloatfloat cam 0 0 1000
        
        -- create ogre head
        ogre <- liftIO $ addEntity bs "ogrehead.mesh"
        
        return (ogre)


myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- initialize the world (and ogre)
        ogreaHead <- liftIO $ initWorld bs smgr
 
        -- get the mouse position Behavior :: Behavior t Vec3
        let posB = getMousePosB bs
 
        -- set ogre head position to mouse position Behavior
        setPosB bs ogreaHead posB

        
        
        
