{-# language OverloadedStrings, RecordWildCards #-}

module Editor.Pickle.LevelFile (
    LevelFile(..),
    mkStandardLevel,
    mkUserLevel,
    mkEpisodeLevel,
    mkUnknownLevel,
    getAbsoluteFilePath,
    isUserLevel,
    isTemplateLevel,
    levelMetaData,
    LevelUID,
    levelUID,
    LevelMetaData(..),
    showLevelTreeForMenu,
    showLevelForMenu,
    showOutroLevelForMenu,
  ) where


import Data.SelectTree
import qualified Data.Map as Map

import System.FilePath
import System.Directory

import Utils

import Base

import StoryMode.Types
import StoryMode.Configuration


mkStandardLevel :: FilePath -> FilePath -> IO LevelFile
mkStandardLevel levelDir levelFile = do
    (dir, package, file) <- mkLevelPath levelDir levelFile
    StandardLevel dir package file <$> loadMetaData levelFile

mkUserLevel :: FilePath -> FilePath -> IO LevelFile
mkUserLevel levelDir levelFile = do
    (dir, package, file) <- mkLevelPath levelDir levelFile
    UserLevel dir package file <$> loadMetaData levelFile

mkEpisodeLevel :: FilePath -> FilePath -> IO (Episode LevelFile -> LevelFile)
mkEpisodeLevel dir_ file_ = do
    (dir, package, file) <- mkLevelPath dir_ file_
    m <- loadMetaData (dir </> package </> file)
    return $ \ e -> EpisodeLevel e dir package file m

-- | PRE: levelDir_ `isPrefixOf` levelFile
mkLevelPath levelDir_ levelFile = do
    dir <- canonicalizePath levelDir_
    let withoutDirM = fmap (dropWhile (`elem` pathSeparators)) $
            dropPrefixMay levelDir_ levelFile
    return $ case withoutDirM of
        Nothing -> error ("mkLevelPath: " ++ show (levelDir_, levelFile))
        Just withoutDir ->
            let package = dropFileName withoutDir
                file = takeFileName withoutDir
            in (dir, package, file)

mkUnknownLevel :: FilePath -> IO LevelFile
mkUnknownLevel = return . UnknownLevelType

isUserLevel :: LevelFile -> Bool
isUserLevel UserLevel{} = True
isUserLevel _ = False

isTemplateLevel :: LevelFile -> Bool
isTemplateLevel TemplateLevel{} = True
isTemplateLevel _ = False

showLevelTreeForMenu :: Scores -> SelectTree LevelFile -> Prose
showLevelTreeForMenu highScores (Leaf _label level) = showLevelForMenu highScores level
showLevelTreeForMenu _highScores x = pVerbatim (x ^. labelA)

showLevelForMenu :: Scores -> LevelFile -> Prose
showLevelForMenu highScores level =
    pVerbatim
        (name ++ " " ++ mkScoreString (fmap fromIntegral $ meta_numberOfBatteries metaData) mHighScore)
  where
    metaData = levelMetaData level
    name = meta_levelName metaData
    mHighScore = Map.lookup (levelUID level) highScores

showOutroLevelForMenu :: Scores -> Episode LevelFile -> EpisodeScore -> LevelFile -> Prose
showOutroLevelForMenu highScores episode epScore lf =
    pVerbatim
        (name ++ " " ++ scoreString)
  where
    name = meta_levelName $ levelMetaData lf
    scoreString = showScore mTime mCollected mTotal
    mTime = case Map.lookup (levelUID lf) highScores of
        Nothing -> Nothing
        Just Score_1_Tried -> Nothing
        Just Score_1_Passed{..} -> Just scoreTime_
    (mCollected, mTotal) = if not (usedBatteryTerminal epScore)
        then (Nothing, Nothing)
        else (Just (sumOfEpisodeBatteries highScores episode), Just batteryNumberNeeded)
