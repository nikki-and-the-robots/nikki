
-- | Module for human readable text.

module Base.Prose (
    Prose,
    getByteString,
    p,
    pFile,
  ) where


import qualified Data.ByteString as BS

import Codec.Binary.UTF8.Light

import Utils


-- | Type for human readable text.
-- (utf8 encoded)
newtype Prose = Prose BS.ByteString

-- | returns the inner bytestring
getByteString :: Prose -> BS.ByteString
getByteString (Prose x) = x

-- | Converts haskell Strings to human readable text.
-- Will be used for translations in the future.
p :: String -> Prose
p = Prose . encode

-- | Read files and return their content as Prose.
-- Should be replaced with something that supports
-- multiple languages of files.
-- (Needs to be separated from p, because it has to return multiple lines.)
pFile :: FilePath -> IO [Prose]
pFile file =
    map p <$> lines <$> readFile file
