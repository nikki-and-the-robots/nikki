{-# language ViewPatterns #-}

-- | Saves the version

module Version (
    nikkiVersion,
    Version.parseVersion,

    -- re-exports from Data.Version
    showVersion,
    Version(..),
  ) where


import Data.Version

import Text.ParserCombinators.ReadP

import Utils


nikkiVersion :: Version
nikkiVersion = Version version tags
  where
    version = 0 : 2 : 11 : []
    tags = []

parseVersion :: String -> Either String Version
parseVersion (stripWhiteSpaces -> s) =
    case readP_to_S Data.Version.parseVersion s of
        (last -> (v, "")) -> Right v
        x -> Left ("version parse error: " ++ show (s, x))
