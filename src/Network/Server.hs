{-# language ScopedTypeVariables, EmptyDataDecls #-}

-- | Server side of network communication using the 'Protocol' type class.
module Network.Server where


import Prelude hiding (catch)

import Data.Version
import Data.Binary
import Data.BinaryCom
import Data.Time

import Control.DeepSeq
import Control.Exception
import Control.Applicative

import System.Locale

import Network.Fancy (streamServer, sleepForever, ServerSpec(..))
import Network.Client


runServer :: forall clientToServer serverToClient .
    (Protocol clientToServer, Protocol serverToClient) =>
    ServerSpec -> (clientToServer -> IO serverToClient) -> IO ()
runServer spec serve = do
    _ <- streamServer spec inner
    sleepForever
  where
    inner handle address = do
        bc <- binaryCom handle
        catchProtocolErrorsOnServer bc $ do
            clientVersion :: Version <- receiveTO bc
            let serverVersion = protocolVersion (undefined :: serverToClient)
            if clientVersion < serverVersion then do
                serverLog ("client-version too old: " ++ show clientVersion)
                logAndSendError bc $ "Client version too old,\nplease update your game."
              else do
                input :: clientToServer <- receiveTO bc
                serverLog $ take maxLogLength $ showAnonymized input
                logAndSendSuccess bc =<< serve input

catchProtocolErrorsOnServer :: BinaryCom -> IO () -> IO ()
catchProtocolErrorsOnServer bc a =
    flip catch handler $
    flip catch timeout $
    a
  where
    handler :: SomeException -> IO ()
    handler (SomeException e) = do
        logAndSendError bc (show e)

    timeout :: Timeout -> IO ()
    timeout Timeout = do
        serverLog "client time out"


-- * logging

-- | maximal number of characters in one log message
maxLogLength = 500

logAndSendError :: BinaryCom -> String -> IO ()
logAndSendError bc err = logAndSend bc (Left err :: Either String Dummy)

data Dummy
instance NFData Dummy where
    rnf = error "Dummy"
instance Binary Dummy where
    put = error "Dummy"
    get = error "Dummy"
instance Protocol Dummy where
    protocolVersion = error "Dummy"
    showAnonymized = error "Dummy"

logAndSendSuccess :: (Protocol a) => BinaryCom -> a -> IO ()
logAndSendSuccess bc a = logAndSend bc (Right a)

logAndSend :: (Protocol a) => BinaryCom -> Either String a -> IO ()
logAndSend bc x = do
    serverLog $ take maxLogLength $ either id showAnonymized x
    send bc x

serverLog :: String -> IO ()
serverLog msg =
    putStrLn =<< mkLogMsg msg

mkLogMsg msg = do
    time <- formatTime defaultTimeLocale timeFormat <$> getCurrentTime
    return (time ++ " : " ++ msg)

timeFormat = "%Y-%m-%d-%H:%M:%S"
