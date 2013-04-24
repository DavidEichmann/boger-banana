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
initWorld :: Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode,SceneNode)
initWorld bs smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 0 0 500
        
        -- default camera
        cam <- sceneManager_getCamera smgr "PlayerCam"
        camera_setPosition_CameraPfloatfloatfloat cam 0 0 1000
        
        -- create ogre head
        ogre1 <- addEntity bs "ogrehead.mesh"
        ogre2 <- addEntity bs "ogrehead.mesh"
        
        return (ogre1,ogre2)


myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- initialize the world (and ogre node)
        (ogre1, ogre2) <- liftIO $ initWorld bs smgr
 
        -- get the mouse position Behavior posB :: Behavior t Vec3
        let mousePosB = getMousePosB bs
 
        -- set ogre1 position to mouse position Behavior
        setPosB bs ogre1 mousePosB
        
        -- create a circling behavior
        let timeB = getTimeB bs
        let relativeCirclePosB = ((\time -> scale 50 (sin time, cos time, 0)) . (*50)) <$> timeB
        let circlePosB = add <$> mousePosB <*> relativeCirclePosB
        
        -- set ogre2 to circlePosB: circle around the mouse position
        setPosB bs ogre2 circlePosB
        
        -- stop on escape key
        let escE = getKeyDownE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE
        
        
        
