
-- | convenient download functions

module Distribution.AutoUpdate.Download where


import qualified Data.ByteString.Lazy as BS
import Data.Monoid

import Control.Monad.Trans.Error

import Network.Curl.Download
import Network.Curl.Download.Lazy

import Utils

import Base.Types
import Base.Prose
import Base.Application.Widgets.GUILog


-- | Tries to fetch the file with the given path.
-- Returns the content of the downloaded file.
downloadContent :: String -> ErrorT String IO String
downloadContent url = do
    ErrorT $ io (openURIString url)

-- | Tries to download the file with the given path into a given file on disc.
-- Uses mkUrl.
downloadFile :: Application_ sort -> String -> FilePath -> ErrorT String IO ()
downloadFile app url destFile = do
    io $ guiLog app (p "downloading " `mappend` pVerbatim url)
    content <- ErrorT $ io (openLazyURI url)
    io $ BS.writeFile destFile content
