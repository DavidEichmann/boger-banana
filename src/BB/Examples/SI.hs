module BB.Examples.SI
where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types
import OIS.Types

import Reactive.Banana.BOGRE
import Reactive.Banana.BOGRE.OIS (KeyState(..))
import BB.Util.Vec


spaceInvaders :: IO ()
spaceInvaders = runGame myGame


shipSpeed = 400
shotSpeed = 1000
playerInitPos = (0,-300,0)

--alienCols :: Int
alienCols = 10
--alienRows :: Int
alienRows = 30

alienSpacing = 60
alienDropSpeed = 10      -- seconds before aliens drop
aliensStartY = 300
aliensEndY = 250



-- init the world and return the FRP network
initWorld :: Frameworks t => HookedBogreSystem t -> SceneManager -> IO (SceneNode, SceneNode, [[SceneNode]])
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
        aliens <- liftIO $ mapM sequence (replicate (floor alienRows) (replicate (floor alienCols) (addEntity bs "ogrehead.mesh")))
        return (head0, target, aliens)


myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- init the world
        (ship, shot, aliens)<- liftIO $ initWorld bs smgr
        
        let
                tB = getTimeB bs
        
        -- keyboard control (shoot and move)
        let
                velLB = stepper (0,0,0) (
                                (\state -> case state of Up -> (0,0,0); Down -> (-1,0,0)) <$> (getKeyStateE bs KC_LEFT)
                        )
                velRB = stepper (0,0,0) (
                                (\state -> case state of Up -> (0,0,0); Down -> (1,0,0)) <$> (getKeyStateE bs KC_RIGHT)
                        )
                velB = (scale shipSpeed) <$> (add <$> velLB <*> velRB)
                
                playerPosB = velocityToPositionB bs playerInitPos velB
                
        setPosB bs ship playerPosB
        
        -- player shot
        let
                shootE = getKeyDownE bs KC_SPACE
                shotTimeB = stepper 0 (tB <@ shootE)
                shotStartPosB = stepper playerInitPos (playerPosB <@ shootE)
                
                shotPosB = shotPos <$> playerPosB <*> shotTimeB <*> shotStartPosB <*> tB
                shotPos playerPos shotTime shotStartPos time
                        | shotTime == 0         = playerPos
                        | otherwise             = shotStartPos `add` (scale (shotSpeed*(time - shotTime)) (0,1,0))
                
        setPosB bs shot shotPosB
        
        -- aliens positions
        let
                alienRelativePoses = [[scale alienSpacing (x-halfWidth,y,0) | x <- [0..alienCols-1]] | y <- [0..alienRows-1]] where
                        halfWidth = alienCols/2
                
                alienFrontCenterB = alienFrontCenter <$> tB where
                        alienFrontCenter time = (x,y,0) where
                                x = alienSpacing * ( shift)
                                y = aliensStartY - (alienSpacing*dropLevel)
                                dropLevel = (fromIntegral . floor . (\t -> t/alienDropSpeed)) time
                                shift
                                        | even ((floor time) `div` shiftMod)     = (fromIntegral ((floor time) `mod` shiftMod)) - 4.5
                                        | otherwise             = 4.5 - (fromIntegral ((floor time) `mod` shiftMod))
                                shiftMod = 10
                aliensPosB = toAlienPoses <$> alienFrontCenterB where
                        toAlienPoses frontCenter = map (map (add frontCenter)) alienRelativePoses
                posNodeZipB = ((`zip` (concat aliens)) . concat) <$> aliensPosB
        
        setDynamicPosBs bs posNodeZipB
        
        -- stop on escape key
        let escE = getKeyDownE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE
        
        return ()
        
        
        
        {-
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
        
        -- dynamically add heads
        newHeadE <- createNodeOnE bs ("ogrehead.mesh" <$ targetHitE)
        -- delay heads
        let headCountB = accumB 1 ((+1) <$ newHeadE)
        let headsDelaysE = (((,) . (*0.2))  <$> headCountB) <@> newHeadE
        delayedB <- getDynamicDelayedPosBs bs head0PosB headsDelaysE
        setDynamicPosBs bs delayedB
        
        -}
        
        
