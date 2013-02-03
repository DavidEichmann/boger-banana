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
        
        let ubs = unhookBogerSystem bs
        
        let
                createHead :: Frameworks s => BogreSystem -> Vec3 -> Moment s (SceneNode)
                createHead ubs pos = do
                        (_,node) <- liftIO $ addEntity (fst ubs) "ogrehead.mesh"
                        liftIO $ setPosition node pos
                        return node
        
        spaceKeyE <- getKeyDownE bs KC_SPACE
        newHeadE <- execute (FrameworksMoment (createHead ubs (0,0,0))  <$ spaceKeyE)
        let headsB = accumB [] ((:) <$> newHeadE)
        let headCountE = accumE 0 ((+1) <$ newHeadE)
        
        velB <- getMouseVelocityB bs
        --reactimate $ print <$> ( ((\b f -> ("fE ", frameDt f, b)) <$> velB) <@> frameE bs)
        --reactimate $ print <$> ( ((\b f -> ("uwE", frameDt f, b)) <$> velB) <@> _updateWorldE bs)

        
        
        dynamicVelsB <- getDynamicDelayedBs bs velB ((\a -> 0 + a-1) <$> headCountE)
        dynamicVelsE <- changes dynamicVelsB
        let headNodeVelZipE = (zip <$> headsB) <@> dynamicVelsE
        let headNodeVelZipB = stepper [] headNodeVelZipE
        --reactimate $ (print) <$> dynamicVelsE
        --reactimate $ (print . (map snd)) <$> headNodeVelZipE
        setDynamicVelocities bs headNodeVelZipB
        
        
        node1 <- createHead ubs (0,0,0)
        node2 <- createHead ubs (0,0,0)
        velD1B <- getDelayedVelocityB bs velB 1
        velD2B <- getDelayedVelocityB bs velD1B 1
        velD3B <- getDelayedVelocityB bs velD2B 1
        setVelocityB bs node1 velD3B
        setVelocityB bs node2 velB
        
        
        
        {-
                create an IO (node) function that creates new body parts, returning the node.
                this IO function gets called whenever the body colides with the target
                        map the IO function to a collision Event via the "execute" function.
                                this results in a new Event t node that will have an event contianing the node
                                whenever a new bodypart is created.
                                
                Some how need to create the behaviour for the dynamically created nodes (dynamic Event/Behaviour switching)
        -}
        
        -- stop on escape key
        escE <- getKeyDownE bs KC_ESCAPE
        reactimate $ (stopBogre bs) <$ escE
        return ()

        
        
        
