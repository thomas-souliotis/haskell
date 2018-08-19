{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_w6 (
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

bindir     = "/home/tom/Desktop/haskell/w6/.stack-work/install/x86_64-linux/nightly-2017-07-12/8.0.2/bin"
libdir     = "/home/tom/Desktop/haskell/w6/.stack-work/install/x86_64-linux/nightly-2017-07-12/8.0.2/lib/x86_64-linux-ghc-8.0.2/w6-0.1.0.0-W9Ugrx6kTHIl3x6l8azM"
dynlibdir  = "/home/tom/Desktop/haskell/w6/.stack-work/install/x86_64-linux/nightly-2017-07-12/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/tom/Desktop/haskell/w6/.stack-work/install/x86_64-linux/nightly-2017-07-12/8.0.2/share/x86_64-linux-ghc-8.0.2/w6-0.1.0.0"
libexecdir = "/home/tom/Desktop/haskell/w6/.stack-work/install/x86_64-linux/nightly-2017-07-12/8.0.2/libexec"
sysconfdir = "/home/tom/Desktop/haskell/w6/.stack-work/install/x86_64-linux/nightly-2017-07-12/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "w6_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "w6_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "w6_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "w6_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "w6_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "w6_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)