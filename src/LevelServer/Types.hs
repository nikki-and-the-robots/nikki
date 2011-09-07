
module LevelServer.Types where


import Data.Binary
import Data.Version

import Control.Applicative
import Control.DeepSeq

import Base.Types.LevelMetaData


port :: Num n => n
port = 8143

protocolVersion :: Version =
    Version [0, 1] []

instance Binary Version where
    put (Version a b) = putWord8 143 >> put a >> put b
    get = do
        143 <- getWord8
        Version <$> get <*> get

instance NFData Version where
    rnf (Version a b) = rnf a `seq` rnf b


data ClientToServer
    = GetLevelList
    | UploadLevel {
        metaData :: LevelMetaData,
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
    = Error [String]
    | LevelList [String]
    | UploadSucceeded
    | UploadNameClash
  deriving (Show)

instance Binary ServerToClient where
    put (Error l) = putWord8 0 >> put l
    put (LevelList l) = putWord8 1 >> put l
    put UploadSucceeded = putWord8 2
    put UploadNameClash = putWord8 3
    get = do
        c <- getWord8
        case c of
            0 -> Error <$> get
            1 -> LevelList <$> get
            2 -> return UploadSucceeded
            3 -> return UploadNameClash

instance NFData ServerToClient where
    rnf (Error a) = rnf a
    rnf (LevelList a) = rnf a
    rnf UploadSucceeded = ()
    rnf UploadNameClash = ()
