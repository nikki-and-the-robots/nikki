{-# language ScopedTypeVariables #-}

module Distribution.AutoUpdate.Zip (unzipArchive, zipArchive) where


import qualified Data.ByteString.Lazy as BS

import Codec.Archive.Zip

import Control.Monad
import Control.Exception

import System.IO
import System.FilePath
import System.Directory

import Utils


-- | unzips a given archive into a given directory
unzipArchive :: (String -> IO ()) -> FilePath -> FilePath -> IO ()
unzipArchive logCommand zipFile directory = do
    logCommand ("unzipping " ++ zipFile)
    withBinaryFile zipFile $ \ content -> do
        let archive = toArchive content
        forM_ (zEntries archive) $ \ entry -> do
            -- modifying the path of the entry to unpack in a given folder.
            let unpackPath = directory </> normalise (eRelativePath entry)
            print unpackPath
            if isZippedDirectory unpackPath then
                createDirectoryIfMissing True unpackPath
              else
                writeEntry [] entry{eRelativePath = unpackPath}
  where
    isZippedDirectory :: FilePath -> Bool
    isZippedDirectory "" = True
    isZippedDirectory f = last f `elem` pathSeparators

    -- Opens a file and performs the given action on the content.
    -- Makes sure the file handle will be closed.
    withBinaryFile :: FilePath -> (BS.ByteString -> IO a) -> IO a
    withBinaryFile file action = do
        bracket open close $ \ handle -> do
            action =<< BS.hGetContents handle
      where
        open = openBinaryFile file ReadMode
        close h = hClose h >> putStrLn "closed!!!"


-- | zips a given folder recursively into the given zipFile
zipArchive :: (String -> IO ()) -> FilePath -> FilePath -> IO ()
zipArchive logCommand zipFile directory = do
    logCommand ("zipping " ++ directory ++ " to " ++ zipFile)
    archive <- addFilesToArchive [OptRecursive] emptyArchive [directory]
    BS.writeFile zipFile (fromArchive archive)
