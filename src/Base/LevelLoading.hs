
module Base.LevelLoading (
    lookupPlayableLevels,
    lookupEditableLevels,
    getSaveLevelDirectory,
  ) where


import Data.SelectTree

import System.FilePath
import System.Directory

import Utils

import Base.Types
import Base.Paths


-- | Returns all files that can be played.
-- Looks in the freeLevelsDirectory and in data/standard_levels
lookupPlayableLevels :: RM (SelectTree FilePath)
lookupPlayableLevels = do
    standardLevelsDir <- getDataFileName "standard_levels"
    standardLevels <- io $ dirToLevels "standard levels" standardLevelsDir
--     downloadedLevels <- lookupDownloadedLevels
    ownedLevels <- io $ lookupEditableLevels "your levels"
    return $
        addChild ownedLevels $
        addChild standardLevels $
        EmptyNode ""

-- | returns all levels created by the user (that can be edited)
lookupEditableLevels :: String -> IO (SelectTree FilePath)
lookupEditableLevels title =
    dirToLevels title =<< getEditableLevelsDirectory

-- | Return the directory where levels are (and should be) saved.
-- A standard directory is returned (using getAppUserDataDirectory "nikki-free-levels").
-- If the standard directory does not exist, it will be created.
getEditableLevelsDirectory :: IO FilePath
getEditableLevelsDirectory = do
    freeLevelsDirectory <- (</> "userLevels") <$> getAppUserDataDirectory "nikki-free-levels"
    createDirectoryIfMissing True freeLevelsDirectory
    return freeLevelsDirectory

-- | returns the directory where to save files
getSaveLevelDirectory :: IO FilePath
getSaveLevelDirectory = getEditableLevelsDirectory

-- | looks up all the levels in a given directory
dirToLevels :: String -> FilePath -> IO (SelectTree FilePath)
dirToLevels title dir =
    mapLabels (dropPrefix dir >>> (title ++) >>> dropExtension) <$>
        readDirectoryToSelectTree ((".nl" ==) . takeExtension) dir
