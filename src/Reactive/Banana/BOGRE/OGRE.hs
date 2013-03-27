--
-- Insert Documentation here :-)
--
module Reactive.Banana.BOGRE.OGRE where



import Reactive.Banana
import Reactive.Banana.Frameworks (
                Frameworks,
                AddHandler,
                fromAddHandler,
                newAddHandler
        )

import Control.Concurrent (
                forkIO
        )




import System.Exit
import Control.Monad

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types

import BB.Workarounds


type Position = (Float,Float,Float)

data World = World {
                worldObject :: SceneNode,
                worldObjectPosition :: Position
        }
        
data DisplaySystem = DisplaySystem {
                window :: RenderWindow,
                root :: Root,
                sceneManager :: SceneManager
        }

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
createDisplaySystem :: IO(DisplaySystem)
createDisplaySystem = do
        -- construct Ogre::Root
        root <- root_new "plugins.cfg" "ogre.cfg" "Ogre.log"

        -- setup resources
        root_addResourceLocation root "../Media" "FileSystem" "Group" True
        
        -- configure
        -- show the configuration dialog and initialise the system
        restored <- root_restoreConfig root
        when (not restored) $ do
                configured <- root_showConfigDialog root
                when (not configured) $ exitWith (ExitFailure 1)
        window <- root_initialise root True "Render Window" ""
        
        -- set default mipmap level (some APIs ignore this)
        root_getTextureManager root >>= \tmgr -> textureManager_setDefaultNumMipmaps tmgr 5
        
        -- initialise all resource groups
        resourceGroupManager_getSingletonPtr >>= resourceGroupManager_initialiseAllResourceGroups
        
        -- create the scene manager, here a generic one
        smgr <- root_createSceneManager_RootPcharPcharP root "DefaultSceneManager" "Scene Manager"
        
        -- create and position the camera
        cam <- sceneManager_createCamera smgr "PlayerCam"
        frustum_setNearClipDistance (toFrustum cam) 5
        
        -- create one viewport, entire window
        vp <- renderTarget_addViewport (toRenderTarget window) cam 0 0 0 1 1
        colourValue_with 0 0 0 1 $ viewport_setBackgroundColour vp
        
        -- Alter the camera aspect ratio to match the viewport
        vpw <- viewport_getActualWidth vp
        vph <- viewport_getActualHeight vp
        frustum_setAspectRatio (toFrustum cam) (fromIntegral vpw / fromIntegral vph)
        
        -- AddHandler for the render Event
        (renderAH, fireRenderEvent) <- newAddHandler
        
        return DisplaySystem {
                        window = window,
                        root = root,
                        sceneManager = smgr
                }
                                
closeDisplaySystem :: DisplaySystem -> IO ()
closeDisplaySystem = root_delete . root
                
nullHandler :: Root -> Float -> () -> IO ((), Bool)
nullHandler _ _ _ = return ((), True)

addEntity :: DisplaySystem -> String -> IO (Entity, SceneNode)
addEntity ds mesh = do
        let smgr = sceneManager ds
        ent <- sceneManager_createEntity_SceneManagerPcharP smgr mesh
        rootNode <- sceneManager_getRootSceneNode smgr
        node <- sceneManager_createSceneNode_SceneManagerP smgr
        node_addChild (toNode rootNode) (toNode node)
        sceneNode_attachObject node (toMovableObject ent)
        return (ent, node)

getPosition :: SceneNode -> IO (Float, Float, Float)
getPosition sn = do
        pos <- node_getPosition (toNode sn)
        x <- getVector3X pos
        y <- getVector3Y pos
        z <- getVector3Z pos
        return (x,y,z)

setPosition :: SceneNode -> (Float, Float, Float) -> IO ()
setPosition sn (x,y,z) = node_setPosition (toNode sn) x y z

setPositionRelative :: SceneNode -> (Float, Float, Float) -> IO ()
setPositionRelative sn (x,y,z) = node_translate_NodePfloatfloatfloatNodeTransformSpace (toNode sn) x y z TS_WORLD

render :: RenderWindow -> Root -> a -> (Root -> Float -> Float -> a -> IO (a, Bool)) -> IO a
render win root value fun = do
    timer <- root_getTimer root
    time <- timer_getMicroseconds timer
    render' time win root value fun

render' :: Int -> RenderWindow -> Root -> a -> (Root -> Float -> Float -> a -> IO (a, Bool)) -> IO a
render' time win root value fun = 
  do windowEventUtilities_messagePump
     closed <- renderWindow_isClosed win
     if closed
       then return value
       else do
         success <- root_renderOneFrame_RootP root
         timer <- root_getTimer root
         time' <- timer_getMicroseconds timer
         let tiF = (fromIntegral (time)) / 1000000
         let tfF = (fromIntegral (time')) / 1000000
         (value', cont) <- fun root tiF tfF value
         if success && cont
           then render' time' win root value' fun
           else return value'




{-
create3DWindow :: Int -> IO (InputSystem)
create3DWindow hwnd = do
        im <- inputManager_createInputSystem_size_t hwnd
        -- get mouse and keyboard objects
        -- unsafeCoerce instead of static_cast
        mouse <- unsafeCoerce $ inputManager_createInputObject im OISMouse True ""
        keyboard <- unsafeCoerce $ inputManager_createInputObject im OISKeyboard True ""
        -- create the addhandlers
        mouseNewAddHandler <- newAddHandler
        keyboardNewAddHandler <- newAddHandler
        -- done, package into a InputSystem
        return ((keyboard, keyboardNewAddHandler), (mouse, mouseNewAddHandler)) -}









