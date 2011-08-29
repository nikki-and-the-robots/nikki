
module StoryMode.Episode where


import System.FilePath

import Utils

import Base

import Editor.Pickle.LevelFile

import StoryMode.Types
import StoryMode.Configuration



loadEpisodes :: IO (Maybe [Episode LevelFile])
loadEpisodes = do
    mEpPath <- getStoryModeLevelsPath
    case mEpPath of
        Nothing -> return Nothing
        Just epPath ->
            Just <$> fmapM (loadEpisode epPath) episodes
  where
    loadEpisode :: FilePath -> Episode String -> IO (Episode LevelFile)
    loadEpisode epPath e = fmapM (loadFile epPath (epPathSnippet $ euid e)) e
    loadFile :: FilePath -> String -> String -> IO LevelFile
    loadFile epPath pathSnippet name =
        let levelPath = epPath </> pathSnippet
            levelFile = levelPath </> name <.> "nl"
        in mkEpisodeLevel levelPath levelFile
