module Paths_homework1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/aprats/Documents/git/cis194-2015-homework/homework1/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/bin"
libdir     = "/Users/aprats/Documents/git/cis194-2015-homework/homework1/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/lib/x86_64-osx-ghc-7.10.3/homework1-0.1.0.0-GjIv9IYxXA88Y5uPMKSJy5"
datadir    = "/Users/aprats/Documents/git/cis194-2015-homework/homework1/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/share/x86_64-osx-ghc-7.10.3/homework1-0.1.0.0"
libexecdir = "/Users/aprats/Documents/git/cis194-2015-homework/homework1/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/libexec"
sysconfdir = "/Users/aprats/Documents/git/cis194-2015-homework/homework1/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "homework1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "homework1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "homework1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "homework1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "homework1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
