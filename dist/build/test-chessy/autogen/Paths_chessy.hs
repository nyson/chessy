{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_chessy (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nyson/.cabal/bin"
libdir     = "/home/nyson/.cabal/lib/x86_64-linux-ghc-8.6.4/chessy-0.1.0.0-1PAX4a4joqt2doQsq5ubdU-test-chessy"
dynlibdir  = "/home/nyson/.cabal/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/nyson/.cabal/share/x86_64-linux-ghc-8.6.4/chessy-0.1.0.0"
libexecdir = "/home/nyson/.cabal/libexec/x86_64-linux-ghc-8.6.4/chessy-0.1.0.0"
sysconfdir = "/home/nyson/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chessy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chessy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chessy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chessy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chessy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chessy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
