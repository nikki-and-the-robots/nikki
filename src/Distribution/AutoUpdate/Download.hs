
-- | convenient download functions

module Distribution.AutoUpdate.Download where


import qualified Data.ByteString.Lazy as BS

import Control.Monad.Trans.Error

import Network.Curl.Download
import Network.Curl.Download.Lazy

import Utils

import Base.Application
import Base.Application.GUILog


-- | Tries to fetch the file with the given path.
-- Returns the content of the downloaded file.
downloadContent :: String -> ErrorT String IO String
downloadContent url = do
    reverseStack $ openURIString url

-- | Tries to download the file with the given path into a given file on disc.
-- Uses mkUrl.
downloadFile :: Application_ sort -> String -> FilePath -> ErrorT String IO ()
downloadFile app url destFile = do
    io $ guiLog app ("downloading " ++ url)
    content <- reverseStack $ openLazyURI url
    io $ BS.writeFile destFile content


-- | reverses the monad stack (just look at the type, willya?)
reverseStack :: IO (Either String a) -> ErrorT String IO a
reverseStack action = do
    x <- io $ action
    case x of
        Left msg -> throwError msg
        Right r -> return r
