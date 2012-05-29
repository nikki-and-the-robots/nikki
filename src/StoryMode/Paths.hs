
module StoryMode.Paths where


import System.Directory
import System.FilePath

import Utils


getStoryModePath :: IO (Maybe FilePath)
getStoryModePath = do
    dir <- getAppUserDataDirectory "nikki-story-mode"
    exists <- doesDirectoryExist dir
    return $ if exists then Just dir else Nothing

createStoryModePath :: IO FilePath
createStoryModePath = do
    dir <- getAppUserDataDirectory "nikki-story-mode"
    createDirectory dir
    return dir

getStoryModeDataFileName :: FilePath -> IO (Maybe FilePath)
getStoryModeDataFileName path =
    fmap (</> ("data" </> path)) <$> getStoryModePath

getStoryModeDataFiles :: FilePath -> Maybe String -> IO (Maybe [FilePath])
getStoryModeDataFiles path_ extension = do
    mPath <- getStoryModeDataFileName path_
    case mPath of
        Nothing -> return Nothing
        Just path -> Just <$> map (path </>) <$> io (getFiles path extension)

getStoryModeLevelsPath :: IO (Maybe FilePath)
getStoryModeLevelsPath =
    fmap (</> "levels") <$> getStoryModePath

getStoryModeLoginDataFile :: IO (Maybe FilePath)
getStoryModeLoginDataFile =
    fmap (</> "loginData") <$> getStoryModePath

getStoryModeVersionFile :: IO (Maybe FilePath)
getStoryModeVersionFile =
    fmap (</> "version") <$> getStoryModePath
