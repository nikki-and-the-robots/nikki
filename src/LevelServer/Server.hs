{-# language ScopedTypeVariables #-}

module LevelServer.Server where

import Prelude hiding (catch)

import Data.Version
import Data.BinaryCom
import Data.Time
import Data.Typeable

import Control.DeepSeq
import Control.Exception
import Control.Applicative

import System.Locale
import System.Timeout

import Network
import Network.Fancy (streamServer, sleepForever, serverSpec, ServerSpec(..), Address(..))

import LevelServer.Types
import LevelServer.Configuration
import LevelServer.Networking


spec = serverSpec{address = IP "0.0.0.0" levelServerPort}

runServer :: (ClientToServer -> IO ServerToClient) -> IO ()
runServer serve = do
    _ <- streamServer spec inner
    sleepForever
  where
    inner handle address = do
        bc <- binaryCom handle
        catchProtocolErrorsOnServer bc $ do
            clientVersion :: Version <- receiveTO bc
            if clientVersion < protocolVersion then do
                serverLog ("client-version too old: " ++ show clientVersion)
                logAndSend bc $ Error ["Client version too old,", "please update your game."]
              else do
                input :: ClientToServer <- receiveTO bc
                serverLog $ take maxLogLength $ show input
                logAndSend bc =<< serve input

catchProtocolErrorsOnServer bc a =
    flip catch handler $
    flip catch timeout $
    a
  where
    handler :: SomeException -> IO ()
    handler (SomeException e) = do
        logAndSend bc $ Error [show e]

    timeout :: Timeout -> IO ()
    timeout Timeout = do
        serverLog "client time out"


-- * logging

-- | maximal number of characters in one log message
maxLogLength = 120


logAndSend bc x = do
    serverLog $ take maxLogLength $ show x
    send bc x

serverLog :: String -> IO ()
serverLog msg =
    putStrLn =<< mkLogMsg msg

mkLogMsg msg = do
    time <- formatTime defaultTimeLocale timeFormat <$> getCurrentTime
    return (time ++ " : " ++ msg)

timeFormat = "%Y-%m-%d-%H:%M:%S"
