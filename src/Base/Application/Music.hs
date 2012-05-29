{-# language ViewPatterns, ScopedTypeVariables #-}

module Base.Application.Music (
    startGameBackgroundMusic,
    stopGameBackgroundMusic,
    pauseGameBackgroundMusic,
  ) where


import Data.Hashable

import Text.Logging

import System.FilePath
import System.Random

import qualified Sound.SFML as SFML

import Utils

import Base.Types
import Base.Paths

import StoryMode.Paths


-- | Selects the music to be played.
-- Returns an absolute file path.
selectMusic :: LevelMetaData -> RM (Maybe FilePath)
selectMusic (meta_musicFile -> Just wantedMusic) = io $ do
    allOggs <- getStoryModeDataFiles "music" (Just ".ogg")
    case fmap (filter (takeBaseName >>> (== wantedMusic))) allOggs of
         Just (x : _) -> return $ Just x
         _ -> return Nothing
selectMusic meta@(meta_musicFile -> Nothing) = do
    -- choose a free music file pseudo-randomly with the level name as the seed.
    allOggsFromPublic <- getDataFiles "music" (Just ".ogg")
    return $ randomElement (mkStdGen (hash (meta_levelName meta))) allOggsFromPublic

randomElement :: StdGen -> [a] -> Maybe a
randomElement g [] = Nothing
randomElement g list =
    let i = fst (random g) `mod` length list
    in Just (list !! i)


-- | Starts the game background music. Restarts the music, if it is paused.
startGameBackgroundMusic :: LevelMetaData -> RM ()
startGameBackgroundMusic meta = do
    mMusic :: Maybe FilePath <- selectMusic meta
    let noMusicMsg =
            maybe
            "no music found."
            (\ mf -> "music not found: " ++
                ("music" </> mf <.> "ogg"))
            (meta_musicFile meta)
    maybe
        (logg Error noMusicMsg)
        (\ file -> io $ SFML.playMusicLooped file (Just 0.6))
        mMusic

pauseGameBackgroundMusic :: IO ()
pauseGameBackgroundMusic = SFML.pauseMusic

stopGameBackgroundMusic :: IO ()
stopGameBackgroundMusic = SFML.stopMusic
