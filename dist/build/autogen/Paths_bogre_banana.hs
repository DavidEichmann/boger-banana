module Paths_bogre_banana (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/david/.cabal/bin"
libdir     = "/home/david/.cabal/lib/bogre-banana-0.1/ghc-7.4.2"
datadir    = "/home/david/.cabal/share/bogre-banana-0.1"
libexecdir = "/home/david/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "bogre_banana_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bogre_banana_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bogre_banana_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bogre_banana_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
