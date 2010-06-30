
module Top.Pickle.Types (
    SaveType,
    FileFormat,
    Grounds(..),
    Layer(..),
    PickleObject(..),
    EditorPosition(..),
    SortId(..),
    Indexable(..),
    Index(..),
  ) where


import Data.Indexable

import Base.Grounds

import Object


-- | newest save type
type SaveType = Grounds PickleObject


-- type FileFormat = ByteString.ByteString
type FileFormat = String


