#!/usr/bin/env runhaskell


import Data.List

import Control.Applicative ((<$>))
import Control.Monad

import System.Directory
import System.FilePath
import System.Process
import System.Exit


topModules =
    words
    ("dist/build/autogen/Paths_nikki " ++
     "Base Control Data Editor Game Graphics Main.hs Object Paths Physics Sorts Top Utils")

outputDir = "dist/doc/code"

main = do
    trySystem "./linuxCompile.sh"
    srcFiles <- concat <$> mapM lookupModules topModules
    mapM_ hsColour srcFiles
    haddock

-- | returns all the corresponding filepaths for a module and
-- all its submodules
lookupModules :: String -> IO [FilePath]
lookupModules name = do
    existsFile <- doesFileExist (name <.> "hs")
    let topModules = if existsFile then [name <.> "hs"] else []
    existsDir <- doesDirectoryExist name
    subModules <- if not existsDir then return [] else
        filter (\ f -> takeExtension f == ".hs") <$> getFilesRecursive name
    return (topModules ++ subModules)

hsColour :: FilePath -> IO ()
hsColour f = do
    putStrLn ("HsColour of " ++ f ++ "...")
    trySystem ("mkdir -p " ++ (outputDir </> takeDirectory f))
    trySystem ("HsColour -html -anchor " ++ f ++ " > " ++ (outputDir </> dropExtension f <.> "html"))

haddock :: IO ()
haddock = do
    let c = ("cabal haddock --executables --haddock-option=--ignore-all-exports " ++
             "--haddock-option=--source-module=\"../../../code/%{MODULE/.//}.html\" " ++
             "--haddock-option=--source-entity=\"../../../code/%{MODULE/.//}.html#%{NAME}\" ")
    trySystem c


-- * utils

-- | returns all (unhidden) files in a directory recursively,
-- excluding all directories
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive dir = do
    content <- filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents dir
    let here = map (dir </>) content
    files <- filterM (\ p -> not <$> doesDirectoryExist p) here
    directories <- filterM doesDirectoryExist here
    recursive <- mapM getFilesRecursive directories
    return $ sort (files ++ concat recursive)

trySystem :: String -> IO ()
trySystem cmd = do
    ec <- system cmd
    case ec of
        ExitSuccess -> return ()
        error -> exitWith error
