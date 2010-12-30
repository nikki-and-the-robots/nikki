
-- Saves the version

module Version (
    nikkiVersion,
    showVersion
  ) where


import Data.Version


nikkiVersion :: Version
nikkiVersion = Version version tags
  where
    version = 0 : 2 : []
    tags = []
