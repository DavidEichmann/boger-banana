module BB.Examples.Snake
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import OIS.Types

import Reactive.Banana.BOGRE

import BB.Util.Vec


snake :: IO ()
snake = runGame myGame

-- init the world and return the FRP network
initWorld :: Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode, SceneNode)
initWorld bs smgr = do
        -- create a light
        l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
        light_setPosition_LightPfloatfloatfloat l 0 0 500
        
        -- default camera
        cam <- sceneManager_getCamera smgr "PlayerCam"
        camera_setPosition_CameraPfloatfloatfloat cam 0 0 1000
        
        -- create first head and target
        head0 <- liftIO $ addEntity bs "ogrehead.mesh"
        target <- liftIO $ addEntity bs "ogrehead.mesh"
        
        return (head0, target)


myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- init the world
        (head0, target)<- liftIO $ initWorld bs smgr
        
        -- keyboard control
        let speed = 200
        let velB = stepper (0,0,0) ((scale speed) <$> (
                        ((0,1,0)  <$ (getKeyDownE bs KC_UP))         `union`
                        ((0,-1,0) <$ (getKeyDownE bs KC_DOWN))       `union`
                        ((-1,0,0) <$ (getKeyDownE bs KC_LEFT))       `union`
                        ((1,0,0)  <$ (getKeyDownE bs KC_RIGHT))      `union`
                        ((0,0,-1) <$ (getKeyDownE bs KC_G))           `union`
                        ((0,0,1)  <$ (getKeyDownE bs KC_B))
                ))
        let head0PosB = velocityToPositionB bs (0,0,0) velB
        setPosB bs head0 head0PosB
        
        -- random bounded positions for target
        randomVec3 <- getRandomVec3B
        let randomTargetPosB = ((sub (boxWidthH,boxWidthH,boxWidthH)) . (scale boxWidth)) <$> randomVec3 where
                boxWidthH = 150
                boxWidth = 2*boxWidthH
                
        
        -- target position changes on collision
        initTargetPos <- initial randomTargetPosB
        targetHitE <- sphereCollisionsE bs 40 head0 target
        let targetPosB = stepper initTargetPos (randomTargetPosB <@ targetHitE) where
        setPosB bs target targetPosB
        
        reactimate $ (putStrLn "hit!") <$ targetHitE
        
        -- dynamically add heads
        newHeadE <- createNodeOnE bs targetHitE
        -- delay heads
        let headCountB = accumB 1 ((+1) <$ newHeadE)
        let headsDelaysE = (((,) . (*0.2))  <$> headCountB) <@> newHeadE
        delayedB <- getDynamicDelayedPosBs bs head0PosB headsDelaysE
        setDynamicPositionBs bs delayedB
        
        -- stop on escape key
        let escE = getKeyDownE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE
        return ()
        
        
        
