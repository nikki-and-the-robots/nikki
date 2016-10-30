{-# language ScopedTypeVariables, MultiParamTypeClasses #-}

module LevelServer.Types where

import           Control.DeepSeq
import           Data.Binary
import qualified Data.ByteString.Lazy as BSL
import           Data.Version
import           Network.Client

data ClientToServer
    = GetLevelList
    | UploadLevel {
        metaData :: BSL.ByteString,
        levelData :: String
      }
  deriving (Show)

instance Binary ClientToServer where
    put GetLevelList = putWord8 0
    put (UploadLevel a b) = putWord8 1 >> put a >> put b
    get = do
        c <- getWord8
        case c of
            0 -> return GetLevelList
            1 -> UploadLevel <$> get <*> get

instance NFData ClientToServer where
    rnf GetLevelList = ()
    rnf (UploadLevel a b) = rnf a `seq` rnf b

data ServerToClient
    = LevelList [String]
    | UploadSucceeded
    | UploadNameClash
  deriving (Show)

instance Binary ServerToClient where
    put (LevelList l) = putWord8 0 >> put l
    put UploadSucceeded = putWord8 1
    put UploadNameClash = putWord8 2
    get = do
        c <- getWord8
        case c of
            0 -> LevelList <$> get
            1 -> return UploadSucceeded
            2 -> return UploadNameClash

instance NFData ServerToClient where
    rnf (LevelList a) = rnf a
    rnf UploadSucceeded = ()
    rnf UploadNameClash = ()

instance Protocol ClientToServer where
    protocolVersion _ = Version [0, 3] []
    showAnonymized = show -- all transmitted data will be public

instance Protocol ServerToClient where
    protocolVersion _ = Version [0, 3] []
    showAnonymized = show
