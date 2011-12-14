{-# language ScopedTypeVariables #-}

module Editor.Pickle.MetaData (
    loadMetaData,
    saveMetaData,
  ) where


import Data.ByteString.Lazy as BSL
import Data.Aeson

import Text.Logging

import System.Directory

import Utils

import Base


loadMetaData :: FilePath -> IO LevelMetaData
loadMetaData levelFile = do
    exists <- doesFileExist (metaFile levelFile)
    if not exists then do
        logg Warning ("level meta data file does not exist: " ++ metaFile levelFile)
        return $ LevelMetaData (guessName levelFile) Nothing Nothing
      else do
        metaDataJSON :: BSL.ByteString <- io $ BSL.readFile (metaFile levelFile)
        let result :: Maybe LevelMetaData = decode metaDataJSON
        case result of
            Nothing -> do
                logg Warning ("meta data not parseable: " ++ levelFile)
                return $ LevelMetaData (guessName levelFile) Nothing Nothing
            Just x -> return x
