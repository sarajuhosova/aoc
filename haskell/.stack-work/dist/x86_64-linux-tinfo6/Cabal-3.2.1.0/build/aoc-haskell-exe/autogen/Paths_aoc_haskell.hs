{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_aoc_haskell (
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

bindir     = "/home/sjuhosova/projects/challenges/aoc-haskell/.stack-work/install/x86_64-linux-tinfo6/5f3664a8a5a3ce05df64e78bc0de9f0b277643cd1f2d87bbfa77f58d69cb8105/8.10.7/bin"
libdir     = "/home/sjuhosova/projects/challenges/aoc-haskell/.stack-work/install/x86_64-linux-tinfo6/5f3664a8a5a3ce05df64e78bc0de9f0b277643cd1f2d87bbfa77f58d69cb8105/8.10.7/lib/x86_64-linux-ghc-8.10.7/aoc-haskell-0.1.0.0-ALcrkOFENf14vBzgexJX4o-aoc-haskell-exe"
dynlibdir  = "/home/sjuhosova/projects/challenges/aoc-haskell/.stack-work/install/x86_64-linux-tinfo6/5f3664a8a5a3ce05df64e78bc0de9f0b277643cd1f2d87bbfa77f58d69cb8105/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/sjuhosova/projects/challenges/aoc-haskell/.stack-work/install/x86_64-linux-tinfo6/5f3664a8a5a3ce05df64e78bc0de9f0b277643cd1f2d87bbfa77f58d69cb8105/8.10.7/share/x86_64-linux-ghc-8.10.7/aoc-haskell-0.1.0.0"
libexecdir = "/home/sjuhosova/projects/challenges/aoc-haskell/.stack-work/install/x86_64-linux-tinfo6/5f3664a8a5a3ce05df64e78bc0de9f0b277643cd1f2d87bbfa77f58d69cb8105/8.10.7/libexec/x86_64-linux-ghc-8.10.7/aoc-haskell-0.1.0.0"
sysconfdir = "/home/sjuhosova/projects/challenges/aoc-haskell/.stack-work/install/x86_64-linux-tinfo6/5f3664a8a5a3ce05df64e78bc0de9f0b277643cd1f2d87bbfa77f58d69cb8105/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "aoc_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "aoc_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "aoc_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "aoc_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aoc_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aoc_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
