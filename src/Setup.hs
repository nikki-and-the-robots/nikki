
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


main =
  case os of
    "darwin" -> macMain
    _ -> defaultMain

-- * mac deployment

macMain :: IO ()
macMain = defaultMainWithHooks simpleUserHooks{postBuild = macPostBuild}

macResourcesDir = "resources"

macPostBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
macPostBuild args buildFlags packageDescription localBuildInfo =
    withTemporaryDirectoryCopy "../data" macResourcesDir $ do
        qtLibDir <- trim <$> readProcess "qmake" ["-query", "QT_INSTALL_LIBS"] ""
        let qtNibDir = qtLibDir </> macResourcesDir </> "qt_menu.nib"
        copyDirectory qtNibDir (macResourcesDir </> "qt_menu.nib")
        resources <- map (macResourcesDir </>) <$> getFilesRecursive macResourcesDir
        appBundleBuildHook [macApp resources] args buildFlags packageDescription localBuildInfo

-- | deployment on a mac
macApp :: [FilePath] -> MacApp
macApp resourceFiles = MacApp {
    appName = "nikki",
    appIcon = Just (macResourcesDir </> "png/icon.icns"),
    appPlist = Nothing,
    resources = resourceFiles,
    otherBins = [],
    appDeps = ChaseWithDefaults
  }


-- * utils

-- | returns all (unhidden) files in a directory recursively
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir =
    withWorkingDirectory dir $ do
        map normalise <$> inner "."
  where
    inner dir = do
        content <- map (dir </>) <$> filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents dir
        (directories, files) <- partitionM doesDirectoryExist content
        recursive <- mapM inner $ directories
        return $ sort (files ++ concat recursive)

-- | change the workingdirectory during the execution of the given action
withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir action = do
    wd <- getCurrentDirectory
    (changeWorkingDirectory dir >> action) `finally` changeWorkingDirectory wd

-- | copy a whole directory recursively
-- excluding hidden files
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    allFiles <- getFilesRecursive src
    forM_ allFiles copy
  where
    copy file = do
        createDirectoryIfMissing True (takeDirectory (dst </> file))
        copyFile (src </> file) (dst </> file)

-- | copy a directory, perform a given action, then delete the copy
withTemporaryDirectoryCopy :: FilePath -> FilePath -> IO a -> IO a
withTemporaryDirectoryCopy original copy action = do
    eDir <- doesDirectoryExist copy
    eFile <- doesFileExist copy
    when (eDir || eFile) $
        fail ("directory (or file?) already exists: " ++ copy)
    (copyDirectory original copy >> action) ` finally` removeDirectoryRecursive copy

partitionM :: (a -> IO Bool) -> [a] -> IO ([a], [a])
partitionM p (a : r) = do
    condition <- p a
    (yes, no) <- partitionM p r
    return $ if condition then
        (a : yes, no)
      else
        (yes, a : no)
partitionM _ [] = return ([], [])

-- | remove surrounding whitespaces
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
