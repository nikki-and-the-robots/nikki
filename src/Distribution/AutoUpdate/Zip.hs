{-# language ScopedTypeVariables #-}

module Distribution.AutoUpdate.Zip (unzipArchive) where


import qualified Data.ByteString.Lazy as BS

import Codec.Archive.Zip

import Control.Monad

import System.FilePath
import System.Directory

import Utils

-- | unzips a given archive into a given directory
unzipArchive :: Application_ sort -> FilePath -> FilePath -> IO ()
unzipArchive zipFile directory = do
    guiLog app ("unzipping " ++ zipFile)
    archive <- toArchive <$> BS.readFile zipFile
    forM_ (zEntries archive) $ \ entry -> do
        -- modifying the path of the entry to unpack in a given folder.
        let unpackPath = directory </> normalise (eRelativePath entry)
        writeEntry [] entry{eRelativePath = unpackPath}
