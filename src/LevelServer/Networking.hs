{-# language ScopedTypeVariables, DeriveDataTypeable #-}

module LevelServer.Networking where


import Prelude hiding (catch)

import Data.BinaryCom
import Data.Typeable

import Control.DeepSeq
import Control.Exception

import System.Timeout

import Network

import LevelServer.Types
import LevelServer.Configuration

-- this module is used by the level server and shouldn't import Utils or Base or anything similar.


-- | in seconds
receiveTimeout = 10

receiveTO bc = do
    mr <- timeout (receiveTimeout * 10 ^ 6) $ deepseqIOId =<< receive bc
    maybe (throwIO Timeout) return mr

data Timeout = Timeout
  deriving (Show, Typeable)

instance Exception Timeout


-- * client side

-- | Can throw IOException and ErrorCall
askServer :: ClientToServer -> IO ServerToClient
askServer msg = do
    h <- connectTo levelServerHost (PortNumber levelServerPort)
    bc <- binaryCom h
    flushAfter bc $ \ bc -> do
        send bc protocolVersion
        send bc msg
    deepseqIOId =<< receiveTO bc

deepseqIOId :: NFData a => a -> IO a
deepseqIOId r = deepseq r () `seq` return r
