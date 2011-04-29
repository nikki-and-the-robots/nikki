{-# language GeneralizedNewtypeDeriving, ScopedTypeVariables #-}


-- | Module for human readable text.

module Base.Prose (
    Prose(..),
    standardFontColor,
    headerFontColor,
    colorizeProse,
    capitalizeProse,
    getByteString,
    p,
    pVerbatim,
    unP,
    pFile,
  ) where


import Data.Monoid
import qualified Data.ByteString as BS
import Data.Char

import Codec.Binary.UTF8.Light

import Control.Arrow

import Graphics.Qt

import Utils


standardFontColor :: Color = QtColor 70 210 245 255
headerFontColor :: Color = QtColor 10 50 60 255

-- | Type for human readable text.
-- (utf8 encoded)
newtype Prose
    = Prose [(Color, BS.ByteString)]
  deriving (Show, Monoid)

colorizeProse :: Color -> Prose -> Prose
colorizeProse color p =
    Prose $ return $ tuple color $ getByteString p

-- | Returns the content of a Prose as a ByteString
getByteString :: Prose -> BS.ByteString
getByteString (Prose list) = foldr (+>) BS.empty $ fmap snd list

capitalizeProse :: Prose -> Prose
capitalizeProse (Prose list) =
    Prose $ fmap (second capitalizeByteString) list
  where
    capitalizeByteString :: BS.ByteString -> BS.ByteString
    capitalizeByteString = encode . map toUpper . decode

-- | Converts haskell Strings to human readable text.
-- Will be used for translations in the future.
p :: String -> Prose
p = pVerbatim

-- | Convert any (ASCII-) string to Prose without doing any translation.
pVerbatim :: String -> Prose
pVerbatim x = Prose [(standardFontColor, encode x)]

-- | inverse of p
unP :: Prose -> String
unP = decode . getByteString

-- | Read files and return their content as Prose.
-- Should be replaced with something that supports
-- multiple languages of files.
-- (Needs to be separated from p, because it has to return multiple lines.)
pFile :: FilePath -> IO [Prose]
pFile file =
    fmap (Prose . return . tuple standardFontColor) <$> byteStringLines <$> BS.readFile file

byteStringLines :: BS.ByteString -> [BS.ByteString]
byteStringLines =
    inner BS.empty
  where
    inner :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
    inner akk x = case BS.uncons x of
        Nothing -> [akk]
        Just (a, r) | a == 10 -> akk : inner BS.empty (BS.tail x)
        Just (a, r) -> inner (akk `BS.snoc` a) r
