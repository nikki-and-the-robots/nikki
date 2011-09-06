
module LevelServer.Networking where


import Prelude hiding (catch)

import Data.BinaryCom

import Control.Exception
import Control.Applicative

import Network
import Network.Fancy (streamServer, sleepForever, serverSpec, ServerSpec(..), Address(..))

import LevelServer.Types

-- this module is used by the level server and shouldn't import Utils or Base or anything similar.


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


askServer :: ClientToServer -> IO (Either IOException ServerToClient)
askServer msg = flip catch handleException $ do
    h <- connectTo "localhost" (PortNumber port)
    bc <- binaryCom h
    sendFlush bc msg
    Right <$> receive bc
  where
    handleException :: IOException -> IO (Either IOException a)
    handleException = return . Left
