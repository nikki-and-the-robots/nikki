{-# language ViewPatterns, ScopedTypeVariables #-}

module Base.Application.Music (
    startGameBackgroundMusic,
    stopGameBackgroundMusic,
    pauseGameBackgroundMusic,
  ) where


import Data.Hashable

import Text.Logging

import Control.Monad.Reader (ask)

import System.FilePath
import System.Directory
import System.Random

import qualified Sound.SFML as SFML

import Utils

import Base.Constants
import Base.Types
import Base.Configuration
import Base.Paths


-- | Selects the music to be played.
-- Returns an absolute file path.
selectMusic :: LevelMetaData -> RM (Maybe FilePath)
selectMusic (meta_musicFile -> Just wantedMusic) = io $ do
    mOggFile <- getStoryModeDataFileName ("music" </> normalise wantedMusic <.> "ogg")
    case mOggFile of
        -- no story mode installed (shouldn't be happening)
        Nothing -> return Nothing
        Just oggFile -> do
            exists <- doesFileExist oggFile
            return $ if exists
                then Just oggFile
                else Nothing
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
    config <- ask
    mMusic :: Maybe FilePath <- selectMusic meta
    let noMusicMsg =
            maybe
            "no music found."
            (\ mf -> "music not found: " ++
                ("music" </> mf <.> "ogg"))
            (meta_musicFile meta)
    maybe
        (logg Error noMusicMsg)
        (\ file -> io $ SFML.playMusicLooped file (Just (globalMusicVolume * config ^. music_volume)))
        mMusic

pauseGameBackgroundMusic :: IO ()
pauseGameBackgroundMusic = SFML.pauseMusic

stopGameBackgroundMusic :: IO ()
stopGameBackgroundMusic = SFML.stopMusic
