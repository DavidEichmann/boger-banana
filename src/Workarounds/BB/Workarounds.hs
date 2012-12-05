{-# LANGUAGE ForeignFunctionInterface #-}
module BB.Workarounds(
getMouseRelX, 
getMouseRelY, 
getWindowHandler
)

where

import BB.Types
import Control.Monad

import Foreign
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "Workarounds.h MyWorkaroundFunctions_getMouseRelX" c_getMouseRelX :: OIS__Mouse -> IO CInt
getMouseRelX :: OIS__Mouse -> IO Int
getMouseRelX p1 =  liftM fromIntegral $  c_getMouseRelX p1

foreign import ccall "Workarounds.h MyWorkaroundFunctions_getMouseRelY" c_getMouseRelY :: OIS__Mouse -> IO CInt
getMouseRelY :: OIS__Mouse -> IO Int
getMouseRelY p1 =  liftM fromIntegral $  c_getMouseRelY p1

foreign import ccall "Workarounds.h MyWorkaroundFunctions_getWindowHandler" c_getWindowHandler :: Ogre__RenderWindow -> IO CSize
getWindowHandler :: Ogre__RenderWindow -> IO Int
getWindowHandler p1 =  liftM fromIntegral $  c_getWindowHandler p1

