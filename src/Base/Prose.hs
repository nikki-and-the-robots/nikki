{-# language GeneralizedNewtypeDeriving #-}


-- | Module for human readable text.

module Base.Prose (
    Prose,
    getByteString,
    p,
    pVerbatim,
    p',
    pFile,
    proseLines,
  ) where


import Data.Monoid
import qualified Data.ByteString as BS

import Codec.Binary.UTF8.Light

import Utils


-- | Type for human readable text.
-- (utf8 encoded)
newtype Prose = Prose BS.ByteString
  deriving Monoid

-- | returns the inner bytestring
getByteString :: Prose -> BS.ByteString
getByteString (Prose x) = x

-- | Converts haskell Strings to human readable text.
-- Will be used for translations in the future.
p :: String -> Prose
p = Prose . encode

-- | Convert any (ASCII-) string to Prose without doing any translation.
pVerbatim :: String -> Prose
pVerbatim = Prose . encode

-- | inverse of p
p' :: Prose -> String
p' (Prose x) = decode x

-- | Read files and return their content as Prose.
-- Should be replaced with something that supports
-- multiple languages of files.
-- (Needs to be separated from p, because it has to return multiple lines.)
pFile :: FilePath -> IO [Prose]
pFile file =
    proseLines <$> Prose <$> BS.readFile file

-- | like Data.List.lines, but with Prose.
proseLines :: Prose -> [Prose]
proseLines (Prose x) =
    map Prose $ inner BS.empty x
  where
    inner :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
    inner akk x = case BS.uncons x of
        Nothing -> [akk]
        Just (a, r) | a == 10 -> akk : inner BS.empty (BS.tail x)
        Just (a, r) -> inner (akk `BS.snoc` a) r
