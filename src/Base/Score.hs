{-# language ViewPatterns, ScopedTypeVariables #-}

module Base.Score (
    Scores,
    HighScoreFile(..),
    Record(..),
    saveScore,
    getScores,
    setScores,
    getHighScores,
    mkScoreString,
    showScore,
    timeFormat,
    batteryFormat,

    sumOfEpisodeBatteries,
  ) where


import Data.Map as Map (Map, empty, lookup, insert)
import Data.Binary
import Data.Binary.Strict
import Data.Initial
import Data.Accessor

import Text.Printf
import Text.Logging

import System.FilePath
import System.Directory

import Utils

import Base.Paths
import Base.Types

import StoryMode.Types


type Scores = Map LevelUID Score

-- | type representing the Map of HighScores,
-- which will be written to file.
-- HighScoreFile has versioned constructors.
-- Serialisation uses the Binary class.
data HighScoreFile
    = HighScoreFile_0 {
        highScores :: Scores
      }
    | HighScoreFile_1 {
        highScores :: Scores,
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
    put s@(HighScoreFile_0 _) = put $ convertToNewest s
    put (HighScoreFile_1 lhs ehs) = do
        putWord8 1
        put lhs
        put ehs
    get = do
        i <- getWord8
        case i of
            0 -> convertToNewest <$> HighScoreFile_0 <$> get
            1 -> HighScoreFile_1 <$> get <*> get

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
        (Score_1_Passed _scoreTime scoreBatteryPower, oldHighScore)
            | oldHighScore `elem` [Nothing, Just Score_1_Tried] -> do
                setHighScore uid currentScore
                let batteryRecord = if scoreBatteryPower == 0 then NoNewRecord else NewRecord
                return (Nothing, NewRecord, batteryRecord)
        (Score_1_Passed _scoreTime _scoreBatteryPower, Just highScore@Score_1_Passed{}) -> do
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
    batteryCompare 0 _ = LT
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

getHighScores :: IO Scores
getHighScores = highScores <$> getScores

setHighScores :: Scores -> IO ()
setHighScores m = do
    eps <- episodeScores <$> getScores
    let content :: HighScoreFile = HighScoreFile_1 m eps
    setScores content

setHighScore :: LevelUID -> Score -> IO ()
setHighScore uid score = do
    setHighScores . insert uid score =<< getHighScores


-- * pretty printing

mkScoreString :: Maybe Integer -> Maybe Score -> String
mkScoreString _ Nothing =
    -- unplayed levels
    showScore Nothing Nothing Nothing
mkScoreString mBatteries (Just score) =
    inner score
  where
    inner :: Score -> String
    inner Score_1_Tried =
        showScore Nothing Nothing mBatteries
    inner (Score_1_Passed time batteries) =
        showScore (Just time) (Just batteries) mBatteries

showScore :: Maybe Seconds -> Maybe Integer -> Maybe Integer -> String
showScore mTime mCollected mTotal =
    printf "[ %s | %s/%s ]"
        (maybe "--:--:--" timeFormat mTime)
        (maybe "---" batteryFormat mCollected)
        (maybe "---" batteryFormat mTotal)

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


-- * file paths

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


-- * episodes

-- | Adds up all collected batteries for one episode.
-- Does not account for batteries in an currently played level.
sumOfEpisodeBatteries :: Scores -> Episode LevelFile -> Integer
sumOfEpisodeBatteries highscore episode =
    sum $ ftoList $ fmap getBatteryPower episode
  where
    getBatteryPower :: LevelFile -> Integer
    getBatteryPower lf = case Map.lookup (levelUID lf) highscore of
        Nothing -> 0
        Just Score_1_Tried -> 0
        Just (Score_1_Passed _ bp) -> bp
