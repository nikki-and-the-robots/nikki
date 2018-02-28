
module StoryMode.Paths where

import Control.Exception

import System.Directory
import System.FilePath

import Base.Paths.GetDataFileName

import Utils


getStoryModePath :: IO FilePath
getStoryModePath = do
    dir <- getDataFileName "story-mode"
    exists <- doesDirectoryExist dir
    if exists
      then return dir
      else throwIO (ErrorCall "can't find 'nikki-story-mode' directory")

getStoryModeDataFileName :: FilePath -> IO FilePath
getStoryModeDataFileName path =
    (</> ("data" </> path)) <$> getStoryModePath

getStoryModeDataFiles :: FilePath -> Maybe String -> IO [FilePath]
getStoryModeDataFiles path_ extension = do
    path <- getStoryModeDataFileName path_
    map (path </>) <$> io (getFiles path extension)

getStoryModeLevelsPath :: IO FilePath
getStoryModeLevelsPath =
    (</> "levels") <$> getStoryModePath
