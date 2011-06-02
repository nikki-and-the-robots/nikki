{-# language ViewPatterns #-}

-- | Saves the version

module Version (
    nikkiVersion,

    -- re-exports from Data.Version and Utils
    showVersion,
    Version(..),
    Utils.parseVersion,
  ) where


import Data.Version

import Utils


nikkiVersion :: Version
nikkiVersion = Version version tags
  where
    version = 0 : 3 : []
    tags = ["edward"]
