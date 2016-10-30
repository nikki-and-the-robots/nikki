{-# language MultiParamTypeClasses, DeriveDataTypeable, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Network.Client where

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!11!!!
-- this module is used by the level server and
-- shouldn't import Utils or Base or anything similar.
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!11!!!

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.Trans.Error
import           Data.Binary
import           Data.BinaryCom
import           Data.Typeable
import           Data.Version
import           Network
import           System.Timeout
import           Text.Logging

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!11!!!
-- this module is used by the level server and
-- shouldn't import Utils or Base or anything similar.
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!!!11!!!


class (Binary p, NFData p) => Protocol p where
    protocolVersion :: p -> Version -- phantom values
    -- | useful for anonymized logging
    showAnonymized :: p -> String

instance Binary Version where
    put (Version a b) = putWord8 143 >> put a >> put b
    get = do
        143 <- getWord8
        Version <$> get <*> get


-- | Can throw IOException and ErrorCall
askServer :: forall a b . (Protocol a, Protocol b, Show a, Show b) =>
    String -> PortNumber -> a -> ErrorT String IO b
askServer host port msg = ErrorT $ do
    h <- connectTo host (PortNumber port)
    bc <- binaryCom h
    flushAfter bc $ \ bc -> do
        send bc $ protocolVersion msg
        send bc msg
    logg Debug ("sent: " ++ show msg ++ " on port " ++ show port)
    answer <- deepseqIOId =<< receiveTO bc
    logg Debug ("received: " ++ show answer)
    return answer

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
