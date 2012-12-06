{-
This is a fusion of the OIS and OGRE modules
-}
module Reactive.Banana.BOGRE where



import Control.Concurrent (forkIO)

import Reactive.Banana.OIS
import Reactive.Banana.OGRE

type BogreSystem = (DisplaySystem, InputSystem)

startBogreSync :: BogreSystem -> IO ()
startBogreSync (ds, is) = render win r () handler
                where
                        win = window ds
                        r = root ds
                        handler _ td _ = do
                                capture is
                                fireFrameEvent ds td
                                return ((), True)
                
startBogre :: BogreSystem -> IO ()
startBogre bs = do 
        _ <- forkIO $ startBogreSync bs
        return ()

