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

import StoryMode.Types


mkStandardLevel :: FilePath -> FilePath -> IO LevelFile
mkStandardLevel levelDir levelFile =
    StandardLevel levelDir levelFile <$> loadMetaData levelFile

mkUserLevel :: FilePath -> FilePath -> IO LevelFile
mkUserLevel levelDir levelFile =
    UserLevel levelDir levelFile <$> loadMetaData levelFile

mkEpisodeLevel :: FilePath -> FilePath -> IO (Episode LevelFile -> LevelFile)
mkEpisodeLevel levelDir levelFile = do
    m <- loadMetaData levelFile
    return $ \ e -> EpisodeLevel e levelDir levelFile m

mkUnknownLevel :: FilePath -> IO LevelFile
mkUnknownLevel = return . UnknownLevelType

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
