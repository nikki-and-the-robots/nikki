
-- | convenient download functions

module Distribution.AutoUpdate.Download where


import qualified Data.ByteString.Lazy as BS

import Control.Monad.Trans.Error

import System.FilePath

import Network.Curl.Download
import Network.Curl.Download.Lazy

import Utils

import Base.Types
import Base.Prose


-- | Tries to fetch the file with the given path.
-- Returns the content of the downloaded file.
downloadContent :: String -> ErrorT String IO String
downloadContent url = do
    ErrorT $ io (openURIString url)

-- | Tries to download the file with the given path into a given file on disc.
-- Uses mkUrl.
downloadFile :: Application -> (Prose -> IO ()) -> String -> FilePath -> ErrorT String IO ()
downloadFile app logCommand url destFile = do
    io $ logCommand (p "downloading " +> pVerbatim (takeBaseName url))
    content <- ErrorT $ io (openLazyURI url)
    io $ BS.writeFile destFile content
