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
        
--addHead :: Frameworks t => HookedBogreSystem t -> IO (SceneNode)
--addHead  bs = do
--        ogreHeadTwo <- addEntity bs "ogrehead.mesh"
--        return ogreHeadTwo
 
myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- initialize the world
        ogreaHead <- liftIO $ initWorld bs smgr
 
        --liftIO $  putStrLn "Hello World!"
 
        -- get the BogerFrame Event :: Event t BogreFrame
        let fE = frameE bs
 
        -- transform to the time :: Event t Float
        let frameTimeE = frameT <$> fE
 
        --let printTimeIOE = print <$> frameTimeE
 
        --reactimate printTimeIOE
 
        -- transform to a Behavior :: Behavior t Float
        let timeB = stepper 0 frameTimeE
 
        -- transform to a position Behavior :: Behavior t Vec3
        --let posB = (\time -> scale 50 (sin time, cos time, 0)) <$> timeB
 
        let posB = getMousePosB bs
 
        -- set the position of the ogre head to the position Behavior
        setPosB bs ogreaHead posB
 
        return ()