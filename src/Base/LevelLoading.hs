
module Base.LevelLoading (
    lookupLevels,
    getFreeLevelsDirectory,
  ) where


import System.FilePath
import System.Directory

import Utils

import Base.Types
import Base.Paths


-- | Returns all files that can be played.
-- Looks in the freeLevelsDirectory and in data/standard_levels
lookupLevels :: RM [FilePath]
lookupLevels = do
    standardLevels <- getDataFiles "standard_levels" (Just ".nl")
    levelDirectory <- getFreeLevelsDirectory
    userLevels <- map (levelDirectory </>) <$> io (getFiles levelDirectory (Just "nl"))
    return (standardLevels ++ userLevels)

-- | Return the directory where levels are (and should be) saved.
-- A standard directory is returned (using getAppUserDataDirectory "nikki-free-levels").
-- If the standard directory does not exist, it will be created.
getFreeLevelsDirectory :: RM FilePath
getFreeLevelsDirectory = do
    freeLevelsDirectory <- io $ getAppUserDataDirectory "nikki-free-levels"
    exists <- io $ doesDirectoryExist freeLevelsDirectory
    when (not exists) $ do
        -- create directory
        io $ createDirectory freeLevelsDirectory
    return freeLevelsDirectory
