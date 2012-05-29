{-# language OverloadedStrings, ScopedTypeVariables #-}

module Base.Types.LevelMetaData where


import Data.ByteString.Lazy as BSL
import Data.Aeson

import Text.Logging

import Control.Monad
import Control.Applicative
import Control.DeepSeq

import System.FilePath
import System.Directory

import Utils


-- * level meta data

-- | This will be saved as JSON, so it will hopefully be extendable.
-- (This is an experimental alternative to versioned constructors.)
data LevelMetaData
    = LevelMetaData {
        meta_levelName :: String,
        meta_author :: Maybe String,
        meta_basedOn :: Maybe String,
        meta_numberOfBatteries :: Maybe Int,
        meta_musicFile :: Maybe FilePath
      }
  deriving (Eq, Show)

instance NFData LevelMetaData where
    rnf (LevelMetaData a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance ToJSON LevelMetaData where
    toJSON (LevelMetaData meta_levelName meta_author basedOn numberOfBatteries musicFile) =
      object (
        "levelName" .= meta_levelName :
        "author" .= meta_author :
        "basedOn" .= basedOn :
        "numberOfBatteries" .= numberOfBatteries :
        "musicFile" .= musicFile :
        [])

instance FromJSON LevelMetaData where
    parseJSON (Object meta) =
        LevelMetaData <$>
            meta .: "levelName" <*>
            meta .:? "author" <*>
            meta .:? "basedOn" <*>
            meta .:? "numberOfBatteries" <*>
            meta .:? "musicFile"
    parseJSON _ = mzero

guessName :: FilePath -> String
guessName = takeBaseName

metaFile :: FilePath -> FilePath
metaFile a = a <.> "meta"

saveMetaData :: FilePath -- ^ level file (.nl)
    -> LevelMetaData -> IO ()
saveMetaData levelFile meta = BSL.writeFile (metaFile levelFile) (encode meta)

loadMetaData :: FilePath -- ^ level file (.nl)
    -> IO LevelMetaData
loadMetaData levelFile = do
    exists <- doesFileExist (metaFile levelFile)
    if not exists then do
        logg Warning ("level meta data file does not exist: " ++ metaFile levelFile)
        return $ LevelMetaData (guessName levelFile) Nothing Nothing Nothing Nothing
      else do
        metaDataJSON :: BSL.ByteString <- io $ BSL.readFile (metaFile levelFile)
        BSL.length metaDataJSON `deepseq` return ()
        let result :: Maybe LevelMetaData = decode' metaDataJSON
        case result of
            Nothing -> do
                logg Warning ("meta data not parseable: " ++ levelFile)
                return $ LevelMetaData (guessName levelFile) Nothing Nothing Nothing Nothing
            Just x -> return x
