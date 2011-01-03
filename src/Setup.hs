
module Main where


import Data.List
import Data.Char

import Control.Applicative
import Control.Monad
import Control.Exception

import System.Directory
import System.Posix.Directory (changeWorkingDirectory)
import System.FilePath
import System.Info
import System.Process

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.MacOSX

import Utils


main =
  case os of
    "darwin" -> macMain
    _ -> defaultMain

-- * mac deployment

macMain :: IO ()
macMain = do
    defaultMainWithHooks simpleUserHooks{postBuild = macPostBuild}

macResourcesDir = "resources"

macPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
macPostBuild args buildFlags packageDescription localBuildInfo =
    withTemporaryDirectoryCopy "../data" macResourcesDir $ do
        qtLibDir <- trim <$> readProcess "qmake" ["-query", "QT_INSTALL_LIBS"] ""
        let qtNibDir = qtLibDir </> macResourcesDir </> "qt_menu.nib"
        copyDirectory qtNibDir (macResourcesDir </> "qt_menu.nib")
        resources <- map (macResourcesDir </>) <$> getFilesRecursive macResourcesDir
        appBundleBuildHook [macApp resources] args buildFlags packageDescription localBuildInfo
        
        let app = "dist/build/core.app"
            appExecutableDir = app </> "Contents/MacOS"
        copyFile "dist/build/nikki/nikki" (appExecutableDir </> "nikki")
        writeFile (app </> "deployed") ""
        copyDirectory app "./nikki.app"

-- | deployment on a mac
macApp :: [FilePath] -> MacApp
macApp resourceFiles = MacApp {
    -- use core executable for now (this results in correct dependency chasing,
    -- but the game gets started with the wrong executable, restarting after updates
    -- will not work)
    appName = "core",
    appIcon = Just (macResourcesDir </> "png/icon.icns"),
    appPlist = Nothing,
    resources = resourceFiles,
    otherBins = [],
    appDeps = ChaseWithDefaults
  }


-- * utils

-- | copy a directory, perform a given action, then delete the copy
withTemporaryDirectoryCopy :: FilePath -> FilePath -> IO a -> IO a
withTemporaryDirectoryCopy original copy action = do
    eDir <- doesDirectoryExist copy
    eFile <- doesFileExist copy
    when (eDir || eFile) $
        fail ("directory (or file?) already exists: " ++ copy)
    (copyDirectory original copy >> action) `finally` removeDirectoryRecursive copy

-- | remove surrounding whitespaces
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
