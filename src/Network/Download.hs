{-# language ScopedTypeVariables #-}

module Network.Download (
    downloadStrict,
    downloadLazy,
  ) where


import Prelude hiding (catch)

import Data.ByteString.Lazy

import Control.Exception

import Network.HTTP
import Network.URI
import Network.Stream (Result, ConnError(..))


downloadStrict :: String -> IO (Either String String)
downloadStrict = httpGet

downloadLazy :: String -> IO (Either String ByteString)
downloadLazy = httpGet

httpGet :: forall s . HStream s => String -> IO (Either String s)
httpGet url =
    case parseURI url of
        Nothing -> return $ Left ("invalid url: " ++ url)
        Just uri -> do
            result :: Result (Response s) <- catchExceptions $ simpleHTTP (mkRequest GET uri)
            return $ case result of
                Left err -> Left $ show err
                Right response -> case rspCode response of
                    (2, 0, 0) -> Right $ rspBody response
                    _ -> Left ("http request failed: " ++ show (rspCode response))

catchExceptions :: IO (Result a) -> IO (Result a)
catchExceptions cmd =
    catch cmd handler
  where
    handler :: SomeException -> IO (Result a)
    handler e = return $ Left $ ErrorMisc (show e)
