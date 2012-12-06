{-
This is a fusion of the OIS and OGRE modules
-}
module Reactive.Banana.BOGRE where



import Control.Concurrent (forkIO)

import qualified Reactive.Banana.OIS as OIS
import Reactive.Banana.OGRE

type BogreSystem = (DisplaySystem, OIS.InputSystem)

startBogreSync :: BogreSystem -> IO ()
startBogreSync (ds, is) = render win r () handler
                where
                        win = window ds
                        r = root ds
                        handler _ td _ = do
                                OIS.capture is
                                fireFrameEvent ds td
                                return ((), True)
                
startBogre :: BogreSystem -> IO ()
startBogre bs = do 
        _ <- forkIO $ startBogreSync bs
        return ()

