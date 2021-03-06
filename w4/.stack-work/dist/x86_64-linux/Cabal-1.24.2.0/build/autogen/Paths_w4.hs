{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_w4 (
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

bindir     = "/home/tom/Desktop/haskell/w4/src/w4/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/bin"
libdir     = "/home/tom/Desktop/haskell/w4/src/w4/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/lib/x86_64-linux-ghc-8.0.2/w4-0.1.0.0-AeXeviOzpZi7LFvYz05J6W"
dynlibdir  = "/home/tom/Desktop/haskell/w4/src/w4/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/tom/Desktop/haskell/w4/src/w4/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/share/x86_64-linux-ghc-8.0.2/w4-0.1.0.0"
libexecdir = "/home/tom/Desktop/haskell/w4/src/w4/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/libexec"
sysconfdir = "/home/tom/Desktop/haskell/w4/src/w4/.stack-work/install/x86_64-linux/lts-9.1/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "w4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "w4_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "w4_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "w4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "w4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "w4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
