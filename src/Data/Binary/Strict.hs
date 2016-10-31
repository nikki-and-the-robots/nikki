
module Data.Binary.Strict where

import           Control.Exception
import           Data.Binary
import           Data.ByteString as Strict
import           Data.ByteString.Lazy as Lazy

-- | returns Nothing, if the saved value cannot be serialised
decodeFileStrict :: Binary a => FilePath -> IO (Maybe a)
decodeFileStrict file =
    decodeMaybe . strictToLazy =<< Strict.readFile file

decodeMaybe :: Binary a => Lazy.ByteString -> IO (Maybe a)
decodeMaybe s =
    catch (Just <$> evaluate (decode s)) handler
  where
    handler :: ErrorCall -> IO (Maybe a)
    handler _e = return Nothing


encodeFileStrict :: Binary a => FilePath -> a -> IO ()
encodeFileStrict file a =
    Strict.writeFile file $ lazyToStrict $ encode a

strictToLazy :: Strict.ByteString -> Lazy.ByteString
strictToLazy = fromChunks . return

lazyToStrict :: Lazy.ByteString -> Strict.ByteString
lazyToStrict = Strict.concat . toChunks
