{-# language OverloadedStrings, RecordWildCards #-}

module Editor.Pickle.LevelFile (
    LevelFile(..),
    mkStandardLevel,
    mkUserLevel,
    mkEpisodeLevel,
    mkUnknownLevel,
    getAbsoluteFilePath,
    levelName,
    isUserLevel,
    isTemplateLevel,
    levelMetaData,
    LevelUID,
    levelUID,
    LevelMetaData(..),
    showLevelTreeForMenu,
    showLevelForMenu,
  ) where


import Data.SelectTree
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad

import System.FilePath
import System.Directory

import Utils

import Base

import Editor.Pickle.MetaData

import StoryMode.Types


mkStandardLevel :: FilePath -> FilePath -> IO LevelFile
mkStandardLevel levelDir levelFile = do
    (dir, package, file) <- mkLevelPath levelDir levelFile
    passThrough print =<< StandardLevel dir package file <$> loadMetaData levelFile

mkUserLevel :: FilePath -> FilePath -> IO LevelFile
mkUserLevel levelDir levelFile = do
    (dir, package, file) <- mkLevelPath levelDir levelFile
    UserLevel dir package file <$> loadMetaData levelFile

mkEpisodeLevel :: FilePath -> FilePath -> IO (Episode LevelFile -> LevelFile)
mkEpisodeLevel dir_ file_ = do
    (dir, package, file) <- mkLevelPath dir_ file_
    m <- loadMetaData (dir </> package </> file)
    return $ \ e -> EpisodeLevel e dir package file m

mkLevelPath levelDir_ levelFile_ = do
    dir <- canonicalizePath levelDir_
    levelFile <- canonicalizePath levelFile_
    let withoutDir = dropWhile (`elem` pathSeparators) $ dropPrefix dir levelFile
        package = dropFileName withoutDir
        file = takeFileName withoutDir
    return (dir, package, file)

mkUnknownLevel :: FilePath -> IO LevelFile
mkUnknownLevel = return . UnknownLevelType

getAbsoluteFilePath :: LevelFile -> FilePath
getAbsoluteFilePath (TemplateLevel p) = p
getAbsoluteFilePath (UnknownLevelType p) = p
getAbsoluteFilePath x = levelPath x </> levelPackage x </> levelFileName x

levelName :: LevelFile -> String
levelName StandardLevel{..} = meta_levelName levelMetaData_
levelName UserLevel{..}     = meta_levelName levelMetaData_
levelName EpisodeLevel{..}  = meta_levelName levelMetaData_

isUserLevel :: LevelFile -> Bool
isUserLevel UserLevel{} = True
isUserLevel _ = False

isTemplateLevel :: LevelFile -> Bool
isTemplateLevel TemplateLevel{} = True
isTemplateLevel _ = False

levelMetaData :: LevelFile -> LevelMetaData
levelMetaData StandardLevel{..} = levelMetaData_
levelMetaData UserLevel{..} = levelMetaData_
levelMetaData EpisodeLevel{..} = levelMetaData_
levelMetaData file = LevelMetaData (guessName $ getAbsoluteFilePath file) Nothing

showLevelTreeForMenu :: SelectTree LevelFile -> IO Prose
showLevelTreeForMenu (Leaf label level) = showLevelForMenu level
showLevelTreeForMenu x = return $ pVerbatim (x ^. labelA)

showLevelForMenu :: LevelFile -> IO Prose
showLevelForMenu level = do
    let name = levelName level
    highScores <- getHighScores
    return $ case Map.lookup (levelUID level) highScores of
        Nothing -> pVerbatim name
        Just highScore -> pVerbatim (name ++ " " ++ mkScoreString highScore)
