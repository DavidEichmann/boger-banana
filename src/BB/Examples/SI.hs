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
alienRows = 3

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
        shot <- addEntity bs "knot.mesh"
        node_scale (toNode shot) 0.1 0.2 0.1
         
        ship <- addEntity bs "ogrehead.mesh"
        
        let createOgreHeadIO = do
                node <- addEntity bs "robot.mesh"
                node_scale (toNode node) 0.7 0.7 0.7
                radian_with_float (3.1415/2) (\yaw -> node_yaw (toNode node) yaw TS_LOCAL)
                radian_with_float (3.1415) (\roll -> node_roll (toNode node) roll TS_LOCAL) 
                return node
                
        aliens <- mapM sequence (replicate (floor alienRows) (replicate (floor alienCols) createOgreHeadIO))
        
        return (ship, shot, aliens)


myGame :: Frameworks t => GameBuilder t
myGame bs smgr = do
        -- init the world
        (ship, shot, aliens) <- liftIO $ initWorld bs smgr
        
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
        
        -- colisions
        let aliensFlat = concat aliens
        alienHitEs <- (mapM (sphereCollisionsE bs 25 shot) (aliensFlat))
        let
                -- an event containing the alien node that was hit by the shot node
                alienHitE = snd <$> (unions alienHitEs)
                deadAliensB = accumB [] ((:) <$> alienHitE)
                
        
        -- player shot
        let
                shootE = getKeyDownE bs KC_SPACE
                shotInMotionB = stepper False (
                                (True  <$ shootE) `union`
                                (False <$ alienHitE)
                        )
                shotTimeB = stepper 0 (tB <@ shootE)
                shotStartPosB = stepper playerInitPos (playerPosB <@ shootE)
                
                shotPosB = shotPos <$> shotInMotionB <*> playerPosB <*> shotTimeB <*> shotStartPosB <*> tB
                shotPos shotInMotion playerPos shotTime shotStartPos time
                        | not shotInMotion      = playerPos
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
                posNodeZipNoDeadB = moveDead <$> deadAliensB <*> posNodeZipB where
                        moveDead dead posNodes = map (\(p,n) -> if n `elem` dead then ((-1000,0,0),n) else (p,n)) posNodes 
        
        setDynamicPosBs bs posNodeZipNoDeadB
        
        -- stop on escape key
        let escE = getKeyDownE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE
        
        return ()

        
