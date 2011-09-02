
module LevelServer.Types where


import Data.Binary

import Control.Applicative


port = 8143

data ClientToServer
    = GetLevelList
  deriving (Show)

data ServerToClient
    = LevelList [String]
  deriving (Show)

-- * binary instances

instance Binary ClientToServer where
    put GetLevelList = putWord8 0
    get = do
        0 <- getWord8
        return GetLevelList

instance Binary ServerToClient where
    put (LevelList l) =
        putWord8 0 >>
        put l
    get = do
        0 <- getWord8
        LevelList <$> get
