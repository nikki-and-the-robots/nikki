{-# language StandaloneDeriving, ScopedTypeVariables #-}

module LevelServer.Networking where


import Data.BinaryCom

import Control.Monad

import Network
import Network.Fancy (streamServer, sleepForever, serverSpec, ServerSpec(..), Address(..))

import LevelServer.Types


runServer :: (ClientToServer -> IO ServerToClient) -> IO ()
runServer serve = do
    streamServer spec inner
    sleepForever
  where
    inner handle address = do
        print address
        bc <- binaryCom handle
        receive bc >>= serve >>= sendFlush bc


spec = serverSpec{address = IP "localhost" port}


askServer :: ClientToServer -> IO ServerToClient
askServer msg = do
    h <- connectTo "joyridelabs.de" (PortNumber port)
    bc <- binaryCom h
    sendFlush bc msg
    receive bc
