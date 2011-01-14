
module Base.LevelLoading (
    lookupLevels,
    getFreeLevelsDirectory,
  ) where


import Control.Monad

import System.FilePath
import System.Directory

import Utils

import Base.Types
import Base.Monad
import Base.Paths
import Base.Configuration


-- | Returns all files that can be played.
-- Looks in the freeLevelsDirectory and in data/standard_levels
lookupLevels :: RM [FilePath]
lookupLevels = do
    standardLevels <- getDataFiles "standard_levels" (Just ".nl")
    run_in_place_ <- asks run_in_place
    if (not run_in_place_) then do
        levelDirectory <- getFreeLevelsDirectory
        userLevels <- map (levelDirectory </>) <$> io (getFiles levelDirectory (Just "nl"))
        return (standardLevels ++ userLevels)
      else
        return standardLevels

-- | Return the directory where levels are (and should be) saved.
-- If --level-path is set, this will be used.
-- Otherwise a standard directory is returned (using getAppUserDataDirectory "nikki-free-levels").
-- If the standard directory does not exist, it will be initialised
-- with a set of standard levels.
getFreeLevelsDirectory :: RM FilePath
getFreeLevelsDirectory = do
    run_in_place_ <- asks run_in_place
    if run_in_place_ then
        getDataFileName "standard_levels"
      else do
        freeLevelsDirectory <- io $ getAppUserDataDirectory "nikki-free-levels"
        exists <- io $ doesDirectoryExist freeLevelsDirectory
        when (not exists) $ do
            -- create directory
            io $ createDirectory freeLevelsDirectory
        return freeLevelsDirectory
