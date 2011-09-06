
module LevelServer.Types where


import Data.Binary
import Data.Version

import Control.Applicative
import Control.DeepSeq


port :: Num n => n
port = 8143

protocolVersion :: Version =
    Version [0, 1] []

instance Binary Version where
    put (Version a b) = putWord8 143 >> put a >> put b
    get = do
        143 <- getWord8
        Version <$> get <*> get


data ClientToServer
    = GetLevelList
    | UploadLevel {
        metaData :: String,
        levelData :: String
      }
  deriving (Show)

instance Binary ClientToServer where
    put GetLevelList = putWord8 0
    get = do
        0 <- getWord8
        return GetLevelList


data ServerToClient
    = Error [String]
    | LevelList [String]
    | UploadReport [String]
  deriving (Show)

instance Binary ServerToClient where
    put (Error l) = putWord8 0 >> put l
    put (LevelList l) = putWord8 1 >> put l
    put (UploadReport l) = putWord8 2 >> put l
    get = do
        c <- getWord8
        case c of
            0 -> Error <$> get
            1 -> LevelList <$> get
            2 -> UploadReport <$> get

instance NFData ServerToClient where
    rnf (Error a) = rnf a
    rnf (LevelList a) = rnf a
    rnf (UploadReport a) = rnf a
