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
-- initWorld ::Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode)
-- initWorld bs smgr = do
-- init the world and return the FRP network
initWorld ::Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode,SceneNode)
initWorld bs smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 20 80 50
 
        -- load oger head
        ogreHead1 <- addEntity bs "ogrehead.mesh"
        ogreHead2 <- addEntity bs "ogrehead.mesh"
        return (ogreHead1,ogreHead2)
 
myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
         -- initialize the world
        (ogreaHeadOne,ogreaHeadTwo) <- liftIO $ initWorld bs smgr
 
        -- get the mouse position Behavior :: Behavior t Vec3
        let posB = getMousePosB bs
      
       -- get the BogerFrame Event :: Event t BogreFrame
        let fE = frameE bs
 
        -- transform to the time :: Event t Float
        let frameTimeE = frameT <$> fE
 
        -- transform to a Behavior :: Behavior t Float
        let timeB = stepper 0 frameTimeE
 
        -- transform to a position Behavior :: Behavior t Vec3
        let posCircle = (\time -> scale 50 (sin (time*10), cos (time*10), 0)) <$> timeB
        let posCircleAdd = add <$> posCircle <*> posB
     --   let posCircle = (\(x,y,z) -> scale 50 (sin x, cos y, 0)) <$> posB
 
        -- set the position of the ogre head to the position Behavior
        setPosB bs ogreaHeadOne posB
        setPosB bs ogreaHeadTwo posCircleAdd
 
      
      
      
  --      let posMB = (\(x,y,z) -> (-x,y,z)) <$> posB
        
         -- set the position of the ogre head to the mouse position Behavior
   --     setPosB bs ogreaHeadOne posB
      -- setPosB bs ogreaHeadTwo posMB
 
        -- get the escape key event :: Event t KeyState
        let escE = getKeyDownE bs KC_ESCAPE
 
        -- replace each event with stopBogre
        let stopGameIOE = (stopBogre bs) <$ escE
 
        -- do the IO actions when they occur
        reactimate stopGameIOE
 
        return ()
 
        