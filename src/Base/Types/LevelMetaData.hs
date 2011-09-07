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
        meta_author :: Maybe String
      }
  deriving (Eq, Show)

instance Binary LevelMetaData where
    put (LevelMetaData a b) = putWord8 0 >> put a >> put b
    get = do
        0 <- getWord8
        LevelMetaData <$> get <*> get

instance NFData LevelMetaData where
    rnf (LevelMetaData a b) = rnf a `seq` rnf b

instance ToJSON LevelMetaData where
    toJSON (LevelMetaData meta_levelName meta_author) =
      object (
        "levelName" .= meta_levelName :
        "author" .= meta_author :
        [])

instance FromJSON LevelMetaData where
    parseJSON (Object meta) =
        LevelMetaData <$>
        meta .: "levelName" <*>
        meta .:? "author"
    parseJSON _ = mzero

saveMetaData :: FilePath -> LevelMetaData -> IO ()
saveMetaData levelFile meta = BSL.writeFile (metaFile levelFile) (encode meta)

metaFile :: FilePath -> FilePath
metaFile a = a <.> "meta"
