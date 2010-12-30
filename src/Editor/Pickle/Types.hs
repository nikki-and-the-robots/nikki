
module Editor.Pickle.Types (
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

import Base

import Object


-- | newest save type
type SaveType = Grounds PickleObject


-- type FileFormat = ByteString.ByteString
type FileFormat = String


