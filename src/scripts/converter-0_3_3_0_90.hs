#!/usr/bin/env runghc

{-# language DeriveDataTypeable #-}


import Data.Initial
import Data.SelectTree

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader

import System.Console.CmdArgs

import Graphics.Qt

import Base

import Editor.Pickle

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
    let config = defaultConfiguration initial
    flip runReaderT config $ withAllSorts $ \ sortTree -> liftIO $ do
        fs <- files <$> cmdArgs sample
        forM_ fs $ \ file -> do
            putStrLn ("loading " ++ file)
            loaded <- loadByFilePath (leafs sortTree) file
            case loaded of
                Right (DiskLevel grounds Nothing meta) -> do
                    putStrLn ("writing " ++ file)
                    writeObjectsToDisk file meta grounds
                Right (DiskLevel grounds (Just x) _) ->
                    putStrLn ("Is already converted: " ++ file)
                Left errors -> putStrLn ("Warning: " ++ show errors)
