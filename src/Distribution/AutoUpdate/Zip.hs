{-# language ScopedTypeVariables #-}

module Distribution.AutoUpdate.Zip (unzipArchive) where


import Data.Word
import qualified Data.ByteString as BS

import Codec.Archive.LibZip

import System.IO
import System.Directory
import System.FilePath

import Utils


-- | unzips a given archive into a given directory
unzipArchive :: FilePath -> FilePath -> IO ()
unzipArchive zipFile directory = do
  putStrLn ("unzipping " ++ zipFile)
  withArchive [CheckConsFlag] zipFile $ do
    files <- fileNames []
    mapM_ (\ f -> unzipSingleFile f directory) files

unzipSingleFile :: FilePath -> FilePath -> Archive ()
unzipSingleFile file directory =
    if isDir file then
        io $ createDirectory (directory </> file)
      else do
        handle <- io $ openFile (directory </> file) WriteMode
        fromFile [] file $ copy handle
        io $ hClose handle

isDir "" = False
isDir p = last p == '/'

copy :: Handle -> Entry ()
copy handle = do
    chunk :: [Word8] <- readBytes 1024 -- 1024 is pretty arbitrary...
             -- using Word8 for binary files.
    if not $ null chunk then do
        io $ BS.hPutStr handle $ BS.pack chunk
        copy handle
      else
        return ()
