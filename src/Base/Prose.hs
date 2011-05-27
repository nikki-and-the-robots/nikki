{-# language GeneralizedNewtypeDeriving, ScopedTypeVariables #-}


-- | Module for human readable text.

module Base.Prose (
    Prose(..),
    standardFontColor,
    headerFontColor,
    colorizeProse,
    capitalizeProse,
    getText,
    nullProse,
    lengthProse,
    p,
    pVerbatim,
    pv,
    unP,
    pFile,
    batteryChar,
    watchChar,
    brackets,
  ) where


import Data.Monoid
import Data.List as List
import Data.Text as Text

import Control.Arrow

import Graphics.Qt

import Utils


standardFontColor :: Color = QtColor 70 210 245 255
headerFontColor :: Color = QtColor 36 110 128 255

-- | Type for human readable text.
-- (utf8 encoded)
newtype Prose
    = Prose [(Color, Text)]
  deriving (Show, Monoid)

colorizeProse :: Color -> Prose -> Prose
colorizeProse color p =
    Prose $ return $ tuple color $ getText p

-- | Returns the content of a Prose as a Data.Text.Text
getText :: Prose -> Text
getText (Prose list) = Prelude.foldr (+>) empty $ fmap snd list

capitalizeProse :: Prose -> Prose
capitalizeProse (Prose list) =
    Prose $ fmap (second toUpper) list

nullProse :: Prose -> Bool
nullProse (Prose snippets) =
    List.all Text.null $ fmap snd snippets

lengthProse :: Prose -> Int
lengthProse (Prose snippets) =
    sum $ fmap (Text.length . snd) snippets

-- | Converts haskell Strings to human readable text.
-- Will be used for translations in the future.
p :: String -> Prose
p = pVerbatim

-- | Convert any (ASCII-) string to Prose without doing any translation.
pVerbatim :: String -> Prose
pVerbatim x = Prose [(standardFontColor, pack x)]

-- | shortcut for pVerbatim
pv = pVerbatim

-- | inverse of p
unP :: Prose -> String
unP = unpack . getText

-- | Read files and return their content as Prose.
-- Should be replaced with something that supports
-- multiple languages of files.
-- (Needs to be separated from p, because it has to return multiple lines.)
pFile :: FilePath -> IO [Prose]
pFile file =
    fmap (Prose . return . tuple standardFontColor) <$> Text.lines <$> pack <$> readFile file

-- | special characters

batteryChar :: Char
batteryChar = '\128267'

watchChar :: Char
watchChar = '\8986'

-- | put brackets around a string
brackets :: Prose -> Prose
brackets x =
    pVerbatim "[" +> x +> pVerbatim "]"
