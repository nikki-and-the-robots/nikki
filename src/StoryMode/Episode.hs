module StoryMode.Episode where

import Data.Maybe
import Data.Initial
import qualified Data.Map as Map

import System.FilePath

import Utils

import Base

import Editor.Pickle.LevelFile

import StoryMode.Types
import StoryMode.Configuration

loadEpisodes :: IO [Episode LevelFile]
loadEpisodes = do
    epPath <- getStoryModeLevelsPath
    fmapM (loadEpisode epPath) episodes
  where
    loadEpisode :: FilePath -> Episode String -> IO (Episode LevelFile)
    loadEpisode epPath e = do
        epF <- fmapM (loadFile epPath (epPathSnippet $ euid e)) e
        let ep = fmap (\ f -> f ep) epF
        return ep
    loadFile :: FilePath -> String -> String -> IO (Episode LevelFile -> LevelFile)
    loadFile epPath pathSnippet name =
        let levelPath = epPath </> pathSnippet
            levelFile = levelPath </> name <.> "nl"
        in mkEpisodeLevel levelPath levelFile

getEpisodeScore :: EpisodeUID -> IO EpisodeScore
getEpisodeScore euid = do
    m <- episodeScores <$> getScores
    return $ fromMaybe initial (Map.lookup euid m)

setEpisodeScore :: EpisodeUID -> EpisodeScore -> IO ()
setEpisodeScore euid score = do
    s <- getScores
    setScores s{episodeScores = Map.insert euid score (episodeScores s)}
