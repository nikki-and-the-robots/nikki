{-# language OverloadedStrings #-}

module Base.Types.LevelMetaData where


import Data.ByteString.Lazy as BSL
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Aeson

import Control.Monad
import Control.Applicative
import Control.DeepSeq

import System.FilePath


-- * level meta data

-- | This will be saved as JSON, so it will hopefully be extendable.
-- (This is an experimental alternative to versioned constructors.)
data LevelMetaData
    = LevelMetaData {
        meta_levelName :: String,
        meta_author :: Maybe String,
        meta_basedOn :: Maybe String
      }
  deriving (Eq, Show)

instance NFData LevelMetaData where
    rnf (LevelMetaData a b c) = rnf a `seq` rnf b `seq` rnf c

instance ToJSON LevelMetaData where
    toJSON (LevelMetaData meta_levelName meta_author basedOn) =
      object (
        "levelName" .= meta_levelName :
        "author" .= meta_author :
        "basedOn" .= basedOn :
        [])

instance FromJSON LevelMetaData where
    parseJSON (Object meta) =
        LevelMetaData <$>
            meta .: "levelName" <*>
            meta .:? "author" <*>
            meta .:? "basedOn"
    parseJSON _ = mzero

saveMetaData :: FilePath -> LevelMetaData -> IO ()
saveMetaData levelFile meta = BSL.writeFile (metaFile levelFile) (encode meta)

metaFile :: FilePath -> FilePath
metaFile a = a <.> "meta"
