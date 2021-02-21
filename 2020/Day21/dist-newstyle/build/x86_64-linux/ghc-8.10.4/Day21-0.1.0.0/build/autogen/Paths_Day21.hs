{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Day21 (
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

bindir     = "/home/mitch/.cabal/bin"
libdir     = "/home/mitch/.cabal/lib/x86_64-linux-ghc-8.10.4/Day21-0.1.0.0-inplace"
dynlibdir  = "/home/mitch/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/mitch/.cabal/share/x86_64-linux-ghc-8.10.4/Day21-0.1.0.0"
libexecdir = "/home/mitch/.cabal/libexec/x86_64-linux-ghc-8.10.4/Day21-0.1.0.0"
sysconfdir = "/home/mitch/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Day21_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Day21_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Day21_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Day21_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Day21_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Day21_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
