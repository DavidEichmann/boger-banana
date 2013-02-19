module BB.Tutorials.Tutorial01
where

import Graphics.Ogre.Types
import Graphics.Ogre.HOgre
 
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.BOGRE
 
import OIS.Types
import BB.Util.Vec
 
 
main :: IO ()
main = runGame myGame
 
 
-- init the world and return the FRP network
initWorld ::Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode)
initWorld bs smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 20 80 50
 
        -- load oger head
        ogreHead <- addEntity bs "ogrehead.mesh"
        return ogreHead
 
 
myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- initialize the world
        ogreaHead <- liftIO $ initWorld bs smgr
 
        -- get the mouse position Behavior :: Behavior t Vec3
        let posB = getMousePosB bs
 
        -- set the position of the ogre head to the mouse position Behavior
        setPosB bs ogreaHead posB
 
        -- get the escape key event :: Event t KeyState
        let escE = getKeyDownE bs KC_ESCAPE
 
        -- replace each event with stopBogre
        let stopGameIOE = (stopBogre bs) <$ escE
 
        -- do the IO actions when they occur
        reactimate stopGameIOE
 
        return ()