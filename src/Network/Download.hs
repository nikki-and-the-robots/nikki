
module Network.Download (
    downloadStrict,
    downloadLazy,
  ) where


import Data.Bifunctor
import Data.ByteString.Lazy

import Network.HTTP
import Network.URI

import Utils


downloadStrict :: String -> IO (Either String String)
downloadStrict = httpGet

downloadLazy :: String -> IO (Either String ByteString)
downloadLazy = httpGet

httpGet :: HStream s => String -> IO (Either String s)
httpGet url =
    case parseURI url of
        Nothing -> return $ Left ("invalid url: " ++ url)
        Just uri -> second rspBody <$> first show <$>
            simpleHTTP (mkRequest GET uri)
