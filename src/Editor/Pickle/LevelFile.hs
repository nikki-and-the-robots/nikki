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

import System.FilePath

import Utils

import Base

import Editor.Pickle


mkStandardLevel :: [Sort_] -> FilePath -> FilePath -> IO LevelFile
mkStandardLevel = mkLevelWithMetaData StandardLevel

mkUserLevel :: [Sort_] -> FilePath -> FilePath -> IO LevelFile
mkUserLevel = mkLevelWithMetaData UserLevel

mkLevelWithMetaData constructor allSorts levelDir levelFile = do
    eLevel <- loadByFilePath allSorts levelFile
    return $ case eLevel of
        Left msg ->
            error $ ("level file not readable: " ++ levelFile ++ "\n" ++ unlines (map unP msg))
        Right (DiskLevel _ _ meta) ->
            constructor levelDir levelFile meta

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
