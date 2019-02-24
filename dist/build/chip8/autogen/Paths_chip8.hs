{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chip8 (
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
version = Version [0,0,2,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/cs/usr/barak.levy/.cabal/bin"
libdir     = "/cs/usr/barak.levy/.cabal/lib/x86_64-linux-ghc-8.2.2/chip8-0.0.2.0-Bv1ys1SdWxj90isRmswINh-chip8"
dynlibdir  = "/cs/usr/barak.levy/.cabal/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/cs/usr/barak.levy/.cabal/share/x86_64-linux-ghc-8.2.2/chip8-0.0.2.0"
libexecdir = "/cs/usr/barak.levy/.cabal/libexec/x86_64-linux-ghc-8.2.2/chip8-0.0.2.0"
sysconfdir = "/cs/usr/barak.levy/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chip8_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chip8_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chip8_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chip8_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chip8_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chip8_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
