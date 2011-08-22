{-# language OverloadedStrings, RecordWildCards #-}

module Editor.Pickle.LevelFile (
    LevelFile(..),
    mkStandardLevel,
    mkUserLevel,
    mkUnknownLevel,
    levelName,
    isUserLevel,
    isTemplateLevel,
    LevelUID,
    levelUID,
    LevelMetaData(..),
    emptyLevelMetaData,
  ) where


import Data.Aeson

import Text.Logging

import Control.Monad
import Control.Monad.Error

import System.FilePath

import Utils

import Base

import Editor.Pickle
import Editor.Pickle.MetaData


mkStandardLevel :: FilePath -> FilePath -> IO LevelFile
mkStandardLevel = mkLevelWithMetaData StandardLevel

mkUserLevel :: FilePath -> FilePath -> IO LevelFile
mkUserLevel = mkLevelWithMetaData UserLevel

mkLevelWithMetaData constructor levelDir levelFile =
    constructor levelDir levelFile <$> loadMetaData levelFile

mkUnknownLevel :: FilePath -> IO LevelFile
mkUnknownLevel = return . UnknownLevelType

levelName StandardLevel{..} = meta_levelName levelMetaData_
levelName UserLevel{..} = meta_levelName levelMetaData_

isUserLevel :: LevelFile -> Bool
isUserLevel UserLevel{} = True
isUserLevel _ = False

isTemplateLevel :: LevelFile -> Bool
isTemplateLevel TemplateLevel{} = True
isTemplateLevel _ = False
