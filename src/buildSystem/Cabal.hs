
module Cabal where


import Control.Monad

import System.Directory
import System.FilePath

import Build


-- | creates targets for cabal builds
cabalTargets :: String -> FilePath -> CabalOptions -> [Target]
cabalTargets postfix path opts =
    [configure, build]
  where
    configure =
        Target confName [] $
            withCurrentDirectory path $ do
                needsConfiguring_ <- needsConfiguring
                when needsConfiguring_ $
                    trySystem ("cabal configure " ++ cabalOptions opts ++ 
                        " --ghc-options=\"" ++ ghcOptions opts ++ "\"")
    build =
        Target buildName [configure] $
            withCurrentDirectory path $ do
                trySystem "cabal build"

    confName = "configure_" ++ postfix
    buildName = "build_" ++ postfix


-- | returns if the cabal build in the current directory needs (re-)configuring.
needsConfiguring :: IO Bool
needsConfiguring = do
    exists <- doesFileExist "dist/setup-config"
    if not exists then
        return True
      else do
        files <- getDirectoryContents "."
        let cabalFile = head $ filter (\ f -> ".cabal" == takeExtension f) files
        cabalMT <- getModificationTime cabalFile
        distMT <- getModificationTime "dist/setup-config"
        return (distMT < cabalMT)


