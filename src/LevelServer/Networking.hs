
module LevelServer.Networking where


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

-- this module is used by the level server and shouldn't import Utils or Base or anything similar.


spec = serverSpec{address = IP "0.0.0.0" port}

-- | in seconds
receiveTimeout = 10

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
                serverLog $ show input
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

logAndSend bc x = do
    serverLog $ show x
    sendFlush bc x

receiveTO bc = do
    mr <- timeout (receiveTimeout * 10 ^ 6) $ deepseqIOId =<< receive bc
    print mr
    maybe (throwIO Timeout) return mr

data Timeout = Timeout
  deriving (Show, Typeable)

instance Exception Timeout


-- * logging

serverLog :: String -> IO ()
serverLog msg =
    putStrLn =<< mkLogMsg msg

mkLogMsg msg = do
    time <- formatTime defaultTimeLocale timeFormat <$> getCurrentTime
    return (time ++ " : " ++ msg)

timeFormat = "%Y-%m-%d-%H:%M:%S"


-- * client side

-- | Can throw IOException and ErrorCall
askServer :: ClientToServer -> IO ServerToClient
askServer msg = do
    h <- connectTo "localhost" (PortNumber port)
    bc <- binaryCom h
    send bc protocolVersion
    sendFlush bc msg
    deepseqIOId =<< receiveTO bc

deepseqIOId :: NFData a => a -> IO a
deepseqIOId r = deepseq r () `seq` return r
