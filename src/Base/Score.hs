{-# language ViewPatterns, ScopedTypeVariables #-}

module Base.Score (
    HighScoreFile(..),
    Record(..),
    saveScore,
    getScores,
    setScores,
    getHighScores,
    collectedBatteries,
    mkScoreString,
    timeFormat,
    batteryFormat,
  ) where


import Data.Map as Map (Map, empty, lookup, insert)
import Data.Binary
import Data.Binary.Strict
import Data.Initial
import Data.Accessor
import Data.Foldable (toList)
import Data.Maybe

import Text.Printf
import Text.Logging

import System.FilePath
import System.Directory

import Utils

import Base.Paths
import Base.Types

import StoryMode.Types


-- | type representing the Map of HighScores,
-- which will be written to file.
-- HighScoreFile has versioned constructors.
-- Serialisation uses the Binary class.
data HighScoreFile
    = HighScoreFile_0 {
        highScores :: Map LevelUID Score
      }
    | HighScoreFile_1 {
        highScores :: Map LevelUID Score,
        episodeScores :: Map EpisodeUID EpisodeScore
      }
  deriving Show

instance Initial HighScoreFile where
    initial = HighScoreFile_1 empty empty

convertToNewest :: HighScoreFile -> HighScoreFile
convertToNewest (HighScoreFile_0 lhs) = HighScoreFile_1 lhs empty
convertToNewest x = x

-- Binary instance for serialisation
-- (to provide minimal cheating protection).
-- This instance has to work with the versioned
-- constructors!!!
instance Binary HighScoreFile where
    put s@(HighScoreFile_0 x) = put $ convertToNewest s
    put (HighScoreFile_1 lhs ehs) = do
        putWord8 1
        put lhs
        put ehs
    get = do
        i <- getWord8
        case i of
            0 -> convertToNewest <$> HighScoreFile_0 <$> get
            1 -> HighScoreFile_1 <$> get <*> get

-- | returns all collected batteries for one episode
collectedBatteries :: Episode LevelFile -> Map LevelUID Integer -> Integer
collectedBatteries e m =
    sum $ toList $ fmap (getBatteryNumber m) e
  where
    getBatteryNumber :: Map LevelUID Integer -> LevelFile -> Integer
    getBatteryNumber m l =
        fromMaybe 0 (Map.lookup (levelUID l) m)

data Record
    = NoNewRecord
    | NewRecord
    | RecordTied

-- | Checks, if the given score is a new record (time- or battery-wise)
-- If yes, saves the new record.
-- Returns (Maybe oldHighScore, newTimeRecord, newBatteryRecord)
saveScore :: LevelFile -> Score -> IO (Maybe Score, Record, Record)
saveScore (levelUID -> uid) currentScore = do
    highScores <- getHighScores
    let mHighScore = Map.lookup uid highScores
    case (currentScore, mHighScore) of
        (_, Nothing) -> do
            setHighScore uid currentScore
            return (Nothing, NoNewRecord, NoNewRecord)
        (Score_1_Tried, Just highScore) ->
            return (Just highScore, NoNewRecord, NoNewRecord)
        (Score_1_Passed scoreTime scoreBatteryPower, oldHighScore)
            | oldHighScore `elem` [Nothing, Just Score_1_Tried] -> do
                setHighScore uid currentScore
                let batteryRecord = if scoreBatteryPower == 0 then NoNewRecord else NewRecord
                return (Nothing, NewRecord, batteryRecord)
        (Score_1_Passed scoreTime scoreBatteryPower, Just highScore@Score_1_Passed{}) -> do
            let newHighScore =
                    updateRecord timeCompare scoreTimeA currentScore $
                    updateRecord compare scoreBatteryPowerA currentScore $
                    highScore
            when (newHighScore /= highScore) $
                setHighScore uid newHighScore
            return (Just highScore,
                record timeCompare scoreTimeA currentScore highScore,
                record batteryCompare scoreBatteryPowerA currentScore highScore)
  where
    timeCompare a b = swapOrdering $ compare a b
    batteryCompare 0 x = LT
    batteryCompare a b = compare a b

    updateRecord :: Compare a -> Accessor Score a -> Score -> Score -> Score
    updateRecord compare acc current high =
        case compare (current ^. acc) (high ^. acc) of
            GT -> acc ^= (current ^. acc) $ high
            _ -> high

    record :: Compare a -> Accessor Score a -> Score -> Score -> Record
    record compare acc current high =
        case compare (current ^. acc) (high ^. acc) of
            GT -> NewRecord
            EQ -> RecordTied
            LT -> NoNewRecord

type Compare a = a -> a -> Ordering

getScores :: IO HighScoreFile
getScores = do
    filePath <- getHighScoreFilePath
    content :: Maybe HighScoreFile <- decodeFileStrict filePath
    case content of
        Nothing -> do
            logg Warning "highscore file not readable."
            return initial
        Just c -> return c

setScores scores = do
    filePath <- getHighScoreFilePath
    encodeFileStrict filePath scores

getHighScores :: IO (Map LevelUID Score)
getHighScores = highScores <$> getScores

setHighScores :: Map LevelUID Score -> IO ()
setHighScores m = do
    eps <- episodeScores <$> getScores
    let content :: HighScoreFile = HighScoreFile_1 m eps
    setScores content

setHighScore :: LevelUID -> Score -> IO ()
setHighScore uid score = do
    setHighScores . insert uid score =<< getHighScores

mkScoreString :: Maybe Int -> Maybe Score -> String
mkScoreString _ s = show s
mkScoreString (Just numberOfBatteries) (Just (Score_0 t b)) =
    printf ("[ %s | %s/%s ]")
        (timeFormat t) (batteryFormat b)
        (batteryFormat $ fromIntegral numberOfBatteries)
mkScoreString Nothing (Just (Score_0 t b)) =
    printf ("[ %s | %s ]") (timeFormat t) (batteryFormat b)

-- | formats the time (MM:SS:MM)
timeFormat :: Seconds -> String
timeFormat time =
    printf "%02i:%02i:%02i" minutes seconds centiSeconds
  where
    (intSeconds, fractionSeconds) = properFraction time
    intMinutes = floor (time / 60)
    minutes :: Int = min 99 intMinutes
    seconds :: Int = min 59 (intSeconds - (intMinutes * 60))
    centiSeconds :: Int = min 99 $ ceiling (fractionSeconds * 100)

batteryFormat :: Integer -> String
batteryFormat = printf "%03i"

-- | Returns the filepath to the highscore file
-- Initializes the file, if it doesn't exist.
getHighScoreFilePath :: IO FilePath
getHighScoreFilePath = do
    confDir <- getConfigurationDirectory
    let highScoreFilePath = confDir </> "highscores"
    exists <- doesFileExist highScoreFilePath
    when (not exists) $ do
        -- initialize the file
        let content :: HighScoreFile = initial
        encodeFileStrict highScoreFilePath content
    return highScoreFilePath
