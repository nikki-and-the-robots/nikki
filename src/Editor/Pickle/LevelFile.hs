{-# language OverloadedStrings, RecordWildCards #-}

module Editor.Pickle.LevelFile (
    LevelFile(..),
    mkStandardLevel,
    mkUserLevel,
    mkEpisodeLevel,
    mkUnknownLevel,
    levelName,
    isUserLevel,
    isTemplateLevel,
    LevelUID,
    levelUID,
    LevelMetaData(..),
    emptyLevelMetaData,
    showLevelTreeForMenu,
    showLevelForMenu,
  ) where


import Data.SelectTree
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad

import System.FilePath

import Utils

import Base

import Editor.Pickle.MetaData


mkStandardLevel :: FilePath -> FilePath -> IO LevelFile
mkStandardLevel = mkLevelWithMetaData StandardLevel

mkUserLevel :: FilePath -> FilePath -> IO LevelFile
mkUserLevel = mkLevelWithMetaData UserLevel

mkEpisodeLevel :: FilePath -> FilePath -> IO LevelFile
mkEpisodeLevel = mkLevelWithMetaData EpisodeLevel

mkLevelWithMetaData constructor levelDir levelFile =
    constructor levelDir levelFile <$> loadMetaData levelFile

mkUnknownLevel :: FilePath -> IO LevelFile
mkUnknownLevel = return . UnknownLevelType

levelName :: LevelFile -> Maybe String
levelName StandardLevel{..} = meta_levelName levelMetaData_
levelName UserLevel{..}     = meta_levelName levelMetaData_
levelName EpisodeLevel{..}  = meta_levelName levelMetaData_

isUserLevel :: LevelFile -> Bool
isUserLevel UserLevel{} = True
isUserLevel _ = False

isTemplateLevel :: LevelFile -> Bool
isTemplateLevel TemplateLevel{} = True
isTemplateLevel _ = False

showLevelTreeForMenu :: SelectTree LevelFile -> IO Prose
showLevelTreeForMenu (Leaf label level) = showLevelForMenu level
showLevelTreeForMenu x = return $ pVerbatim (x ^. labelA)

showLevelForMenu :: LevelFile -> IO Prose
showLevelForMenu level = do
    let name = fromMaybe "???" $ levelName level
    highScores <- getHighScores
    return $ case Map.lookup (levelUID level) highScores of
        Nothing -> pVerbatim name
        Just highScore -> pVerbatim (name ++ " " ++ mkScoreString highScore)
