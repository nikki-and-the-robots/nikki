{-# language MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}

-- this module is used by the level server and
-- shouldn't import Utils or Base or anything similar.

module Network.Client where


import Data.Version
import Data.BinaryCom
import Data.Typeable
import Data.Binary

import Control.Applicative
import Control.DeepSeq
import Control.Exception

import System.Timeout

import Network


class (Binary a, Binary b, NFData b) => Protocol a b where
    protocolVersion :: a -> b -> Version -- phantom values

instance Binary Version where
    put (Version a b) = putWord8 143 >> put a >> put b
    get = do
        143 <- getWord8
        Version <$> get <*> get

instance NFData Version where
    rnf (Version a b) = rnf a `seq` rnf b


-- | Can throw IOException and ErrorCall
askServer :: forall a b . Protocol a b => String -> PortNumber -> a -> IO b
askServer host port msg = do
    h <- connectTo host (PortNumber port)
    bc <- binaryCom h
    flushAfter bc $ \ bc -> do
        send bc $ protocolVersion (undefined :: a) (undefined :: b)
        send bc msg
    deepseqIOId =<< receiveTO bc

deepseqIOId :: NFData a => a -> IO a
deepseqIOId r = deepseq r () `seq` return r


-- | in seconds
receiveTimeout = 10

receiveTO bc = do
    mr <- timeout (receiveTimeout * 10 ^ 6) $ deepseqIOId =<< receive bc
    maybe (throwIO Timeout) return mr

data Timeout = Timeout
  deriving (Show, Typeable)

instance Exception Timeout
