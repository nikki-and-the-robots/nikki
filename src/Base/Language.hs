
module Base.Language where


import Data.Data


-- | Versioned language identifiers
data Language
    = English | German
  deriving (Show, Read, Typeable, Data)
