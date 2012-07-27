-- | Saves the version

module Version where


import Data.Version


{-# noinline nikkiVersion #-}
nikkiVersion :: Version
nikkiVersion = Version version tags
  where
    version = 0 : 5 : 1 : 1 : []
    tags = ["samuel"]
