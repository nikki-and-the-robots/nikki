-- | Saves the version

module Version where


import Data.Version


{-# noinline nikkiVersion #-}
nikkiVersion :: Version
nikkiVersion = Version version tags
  where
    version = 1 : 0 : 1 : []
    tags = ["marley"]
