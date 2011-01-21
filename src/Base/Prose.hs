
-- | Module for human readable text.

module Base.Prose (
    Prose,
    getByteString,
    p,
  ) where


import Data.ByteString

import Codec.Binary.UTF8.Light

import Utils


-- | Type for human readable text.
-- (utf8 encoded)
newtype Prose = Prose ByteString

-- | Converts haskell Strings to human readable text.
-- Will be used for translations in the future.
p :: String -> Prose
p = Prose . encode

-- | returns the inner bytestring
getByteString :: Prose -> ByteString
getByteString (Prose x) = x
