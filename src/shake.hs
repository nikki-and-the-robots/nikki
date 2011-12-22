#!/usr/bin/env runghc


import Data.List

import Control.Applicative ((<$>))
import Control.Monad.IO.Class

import System.Directory
import System.FilePath

import Utils.Scripting

import Development.Shake


main = shake shakeOptions{shakeParallel = 2, shakeVerbosity = 2} $ do
    let core = "dist" </> "build" </> "core" </> "core"
        qtWrapper = "cpp" </> "dist" </> "libqtwrapper.a"
        cppMakefile = "cpp" </> "dist" </> "Makefile"
        cabalConfiguration = "dist" </> "setup-config"


    want [core]

    core *> \ _ -> do
        need [qtWrapper, cabalConfiguration]
        hss <- getHaskellSources
        need hss
        liftIO $ print ("NYI: too much haskell files as deps")
        system' "cabal" ["build"]

    cabalConfiguration *> \ _ -> do
        need [qtWrapper]
        need ["nikki.cabal"]
        system' "cabal" ["configure"]

    qtWrapper *> \ _ -> do
        cs <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.cpp"
        hs <- map ("cpp" </>) <$> getDirectoryFiles "cpp" "*.h"
        need (cppMakefile : cs ++ hs)
        withDirectory ("cpp" </> "dist") $ do
            system' "make" []

    cppMakefile *> \ _ -> do
        need ["cpp" </> "CMakeLists.txt"]
        liftIO $ createDirectoryIfMissing False ("cpp" </> "dist")
        withDirectory ("cpp" </> "dist") $ do
            system' "cmake" [".."]

getHaskellSources :: Action [FilePath]
getHaskellSources =
    filter (".hs" `isSuffixOf`) <$> (liftIO $ getFilesRecursive ".")


haskellSuperModules =
    "Base" :
    "Control" :
    "Data" :
    "Distribution" :
    "Editor" :
    "FakeFutureVersion" :
    "Game" :
    "Graphics" :
    "Legacy" :
    "LevelServer" :
    "Main" :
    "Network" :
    "Object" :
    "Physics" :
    "Profiling" :
    "Setup" :
    "Sorts" :
    "StoryMode" :
    "System" :
    "Text" :
    "Top" :
    "Utils" :
    "Version" :
    []


-- * utils

withDirectory :: MonadIO m => FilePath -> m a -> m a
withDirectory dir action = do
    outer <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory dir
    r <- action
    liftIO $ setCurrentDirectory outer
    return r
