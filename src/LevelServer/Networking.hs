
module LevelServer.Networking where


import Data.BinaryCom

import Control.Monad

import Network
import Network.Fancy (streamServer, sleepForever, serverSpec, ServerSpec(..), Address(..))

import LevelServer.Types


runServer :: (String -> IO ()) -> (ClientToServer -> IO ServerToClient) -> IO ()
runServer serverLog serve = do
    _ <- streamServer spec inner
    sleepForever
  where
    inner handle address = do
        bc <- binaryCom handle
        input <- receive bc
        serverLog $ show input
        output <- serve input
        serverLog $ show output
        sendFlush bc output


spec = serverSpec{address = IP "0.0.0.0" port}


askServer :: ClientToServer -> IO ServerToClient
askServer msg = do
    h <- connectTo "joyridelabs.de" (PortNumber port)
    bc <- binaryCom h
    sendFlush bc msg
    receive bc
