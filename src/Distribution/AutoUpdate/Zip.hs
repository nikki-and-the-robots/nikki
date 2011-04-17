{-# language ScopedTypeVariables #-}

module Distribution.AutoUpdate.Zip (unzipArchive, zipArchive) where


import qualified Data.ByteString as BS
import Data.Word

import Codec.Archive.LibZip

import Control.Monad.CatchIO

import System.FilePath
import System.Directory
import System.IO

import Utils


-- | chunk size while unzipping
chunkSize :: Int
chunkSize = 1024

-- | unzips a given archive into a given directory
unzipArchive :: FilePath -> FilePath -> IO ()
unzipArchive zipFile directory =
    withArchive [] zipFile $ do
        files <- fileNames []
        forM_ files $ extract directory

-- | Extracts a file from an archive to a given directory,
-- including full path. Creates parent directories if necessary.
extract :: FilePath -> FilePath -> Archive ()
extract destination fileInArchive = do
    let fullPath = destination </> fileInArchive
        parent = joinPath $ init $ splitDirectories fullPath
    io $ assertNonExistance fullPath
    io $ createDirectoryIfMissing True parent
    if isDir fileInArchive then
        io $ createDirectory fullPath
      else
        fromFile [] fileInArchive (extractFile fullPath)
  where
    isDir = (== '/') . last

extractFile :: FilePath -> Entry ()
extractFile destination = do
    io $ assertNonExistance destination
    withFileMIO destination WriteMode inner
  where
    inner :: Handle -> Entry ()
    inner handle = do
        chunk :: [Word8] <- readBytes chunkSize
        if not (null chunk) then do
            io $ BS.hPutStr handle (BS.pack chunk)
            inner handle
          else
            return ()

-- | like withFile, but with MonadCatchIO
withFileMIO :: MonadCatchIO m => FilePath -> IOMode -> (Handle -> m a) -> m a
withFileMIO name mode =
    bracket (io $ openFile name mode) (io . hClose)

-- | zips a given folder recursively into the given zipFile
zipArchive :: FilePath -> FilePath -> IO ()
zipArchive zipFile directory = do
    assertNonExistance zipFile
    withArchive [CreateFlag] zipFile $ putInZip directory

-- | asserts that a file or directory does not exist.
assertNonExistance :: FilePath -> IO ()
assertNonExistance file = do
    isFile <- doesFileExist file
    isDir <- doesDirectoryExist file
    when (isFile || isDir) $
        error ("file already exists: " ++ file)

-- | puts the given file or directory (recursively) into the current archive
putInZip :: FilePath -> Archive ()
putInZip file =
    inner (takeDirectory file) (takeFileName file)
  where
    inner root file = do
        let fullPath = root </> file
        isDir <- io $ doesDirectoryExist fullPath
        isFile <- io $ doesFileExist fullPath
        if isDir then do
            ignore $ addDirectory file
            subDirFiles <- io $ getFiles fullPath Nothing
            mapM_ (inner root) $ map (file </>) subDirFiles
          else if isFile then do
            content <- sourceFile fullPath 0 0
            ignore $ addFile file content
            return ()
          else
            error ("file not found: " ++ file)
