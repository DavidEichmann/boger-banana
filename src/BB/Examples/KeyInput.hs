module BB.Examples.KeyInput where

import System.Environment(getEnv)
import Control.Concurrent (threadDelay)

import Graphics.X11.Xlib

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.OIS

-- Create a window and return the Int handle
createDummyWindow :: IO Int
createDummyWindow = do
  displayname <- getEnv "DISPLAY"
  xdisp <- openDisplay displayname -- TODO: check for null ptr
  xwin <- createSimpleWindow xdisp (defaultRootWindow xdisp) 0 0 100 100 0 0 0
  mapWindow xdisp xwin
  selectInput xdisp xwin structureNotifyMask
  allocaXEvent $ \ev -> do
    let fn = do
             nextEvent xdisp ev
             et <- get_EventType ev
             if et /= mapNotify 
               then fn
               else return ()
    fn
  let hwnd = fromIntegral xwin
  return hwnd
                
keyInputExample :: IO ()
keyInputExample = do
        handle <- createDummyWindow
        is <- createInputSystem handle
        eventNet <- compile $ network is
        actuate eventNet
        _ <- startPolling is 1
        -- wait 10 seconds before exiting
        threadDelay 10000000
        
network :: Frameworks t => InputSystem -> Moment t ()
network is = do
        -- input
        keyE <- getKeyE is
        
        -- output
        reactimate $ printKey <$> keyE
        
printKey :: Show a => a -> IO ()
printKey pressedKeys = do
        putStrLn $ "Pressed keys: " ++ (show pressedKeys)