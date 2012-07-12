{-# language ScopedTypeVariables #-}

module Network.Download (
    downloadStrict,
    downloadLazy,
  ) where


import Data.ByteString.Lazy

import Network.HTTP
import Network.URI
import Network.Stream (Result)


downloadStrict :: String -> IO (Either String String)
downloadStrict = httpGet

downloadLazy :: String -> IO (Either String ByteString)
downloadLazy = httpGet

httpGet :: forall s . HStream s => String -> IO (Either String s)
httpGet url =
    case parseURI url of
        Nothing -> return $ Left ("invalid url: " ++ url)
        Just uri -> do
            result :: Result (Response s) <- simpleHTTP (mkRequest GET uri)
            return $ case result of
                Left err -> Left $ show err
                Right response -> case rspCode response of
                    (2, 0, 0) -> Right $ rspBody response
                    _ -> Left ("http request failed: " ++ show (rspCode response))
