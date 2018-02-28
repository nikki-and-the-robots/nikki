{-# language ScopedTypeVariables #-}

module Editor.Pickle.LevelLoading (
    lookupPlayableLevels,
    lookupUserLevels,
    getSaveLevelDirectory,
  ) where


import Data.SelectTree

import System.FilePath
import System.Directory

import Utils

import Base.Types
import Base.Paths

import Editor.Pickle.LevelFile


-- | Returns all files that can be played.
-- Looks in the freeLevelsDirectory and in data/standardLevels
lookupPlayableLevels :: IO (SelectTree LevelFile)
lookupPlayableLevels = do
    standardLevelsDir <- getDataFileName "standardLevels"
    standardLevels :: SelectTree LevelFile <-
        (fmapM (mkStandardLevel standardLevelsDir) =<<
        dirToLevels "standard levels" standardLevelsDir)
--     downloadedLevels <- lookupDownloadedLevels
    ownedLevels <- lookupUserLevels "your levels"
    return $
        addChild ownedLevels $
        addChild standardLevels $
        EmptyNode ""

-- | returns all levels created by the user (that can be edited)
lookupUserLevels :: String -> IO (SelectTree LevelFile)
lookupUserLevels title = do
    userLevelsDir <- getUserLevelsDirectory
    fmapM (mkUserLevel userLevelsDir) =<< dirToLevels title userLevelsDir

-- | Return the directory where levels are (and should be) saved.
-- A standard directory is returned (using getAppUserDataDirectory "nikki-free-levels").
-- If the standard directory does not exist, it will be created.
getUserLevelsDirectory :: IO FilePath
getUserLevelsDirectory = do
    freeLevelsDirectory <- (</> "userLevels") <$> getAppUserDataDirectory "nikki-free-levels"
    createDirectoryIfMissing True freeLevelsDirectory
    return freeLevelsDirectory

-- | returns the directory where to save files
getSaveLevelDirectory :: IO FilePath
getSaveLevelDirectory = getUserLevelsDirectory

-- | looks up all the levels in a given directory
dirToLevels :: String -> FilePath -> IO (SelectTree FilePath)
dirToLevels title dir =
    mapLabels (dropPrefix dir >>> (title ++) >>> dropExtension) <$>
        readDirectoryToSelectTree ((".nl" ==) . takeExtension) dir
