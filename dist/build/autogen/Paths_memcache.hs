module Paths_memcache (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/davidt/.cabal/bin"
libdir     = "/home/davidt/.cabal/lib/memcache-0.1.0.0/ghc-7.4.1.20120416"
datadir    = "/home/davidt/.cabal/share/memcache-0.1.0.0"
libexecdir = "/home/davidt/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "memcache_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "memcache_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "memcache_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "memcache_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
