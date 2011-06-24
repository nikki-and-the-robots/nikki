-- | Saves the version

module Version where


import Data.Version


nikkiVersion :: Version
nikkiVersion = Version version tags
  where
    version = 999 : []
    tags = ["fakeFuture"]
