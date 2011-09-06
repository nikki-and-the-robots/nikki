#!/usr/bin/env runghc

{-# language Ã¶ DeriveDataTypeable #-}


import Data.Initial
import Data.SelectTree
import Data.Maybe

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Error

import System.IO
import System.Console.CmdArgs

import Graphics.Qt

import Base

import Editor.Pickle
import Editor.Pickle.MetaData

import Top.Initialisation


data Args = Args {
    files :: [FilePath]
  }
    deriving (Typeable, Data)

sample = Args {
    files = []
        &= args
        &= typFile
  }


main = withQApplication $ \ qApp -> do
    hSetBuffering stdout NoBuffering
    let config = defaultConfiguration initial
    flip runReaderT config $ withAllSorts $ \ sortTree -> liftIO $ do
        fs <- files <$> cmdArgs sample
        forM_ fs $ \ file -> do
            putStrLn ("loading " ++ file)
            loaded <- runErrorT $ loadByFilePath (leafs sortTree) file
            case loaded of
                Right (DiskLevel grounds Nothing) -> do
                    meta <- loadMetaData file
                    putStr ("author of " ++ fromJust (meta_levelName meta) ++ ": ")
                    author <- getLine
                    putStrLn ("writing " ++ file)
                    writeObjectsToDisk file meta{meta_author = Just author} grounds
                Right (DiskLevel grounds (Just x)) ->
                    putStrLn ("Is already converted: " ++ file)
                Left errors -> putStrLn ("Warning: " ++ show errors)
        quitQApplication
