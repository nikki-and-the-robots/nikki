
-- | convenient download functions

module Distribution.AutoUpdate.Download where


import qualified Data.ByteString.Lazy as BS

import Control.Monad.Trans.Error

import System.FilePath

import Network.Download

import Utils

import Base.Types
import Base.Prose


-- | Tries to fetch the file with the given path.
-- Returns the content of the downloaded file.
downloadContent :: String -> ErrorT String IO String
downloadContent url = do
    ErrorT $ annotateError url $ downloadStrict url

-- | Tries to download the file with the given path into a given file on disc.
-- Uses mkUrl.
downloadFile :: Application -> (Prose -> IO ()) -> String -> FilePath -> ErrorT String IO ()
downloadFile app logCommand url destFile = do
    io $ logCommand (p "downloading " <> pVerbatim (takeFileName url))
    content <- ErrorT $ annotateError url $ downloadLazy url
    io $ BS.writeFile destFile content

annotateError :: String -> IO (Either String a) -> IO (Either String a)
annotateError url cmd = do
    result <- cmd
    return $ case result of
        Left err -> Left (
            "error downloading\n" ++
            unlines (split url) ++
            err)
        Right x -> Right x
  where
    -- splits long URLs in small chunks to be readable
    split x | length x <= limit = [x]
    split x =
        (a ++ " ...") : split b
      where
        (a, b) = splitAt limit x
    limit = 30
