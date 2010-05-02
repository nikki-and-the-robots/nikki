
module Cabal where


import Control.Monad

import System.Directory
import System.FilePath


import Utils

import Build


-- | creates targets for cabal builds
cabalTargets :: String -> FilePath -> [Target]
cabalTargets postfix path =
    [configure, build]
  where
    configure =
        Target confName [] $
            withCurrentDirectory path $ do
                needsConfiguring_ <- needsConfiguring
                when needsConfiguring_ $
                    trySystem "cabal configure"
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
        let cabalFile =
                files
                |> filter (\ f -> ".cabal" == takeExtension f)
                |> head
        cabalMT <- getModificationTime cabalFile
        distMT <- getModificationTime "dist/setup-config"
        return (distMT < cabalMT)


