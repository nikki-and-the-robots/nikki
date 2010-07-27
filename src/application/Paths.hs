
-- | This module replaces cabal's auto-generated Paths_nikki.
-- How exactly this should work should still be considered in flux.
-- At the moment, on windows this module looks for data files in the same directory as 
-- the executable.
-- On Linux it behaves like Paths_nikki.

module Paths (getDataFileName) where


import Control.Applicative

import System.Directory
import System.FilePath

import qualified Paths_nikki as Cabals

import Graphics.Qt


getDataFileName path = do
    hay <- searchPaths
    mFile <- pathSearch hay path
    case mFile of
        Just f -> return f
        Nothing -> error ("Data files not found: " ++ path ++ "\nSearched in:\n" ++ unlines hay)

searchPaths = do
    cabalPath <- Cabals.getDataDir
    executablePath <- dropFileName <$> applicationFilePath
    currentPath <- getCurrentDirectory
    return [cabalPath, executablePath]

-- searches for a subpath in a given list of paths. Returns both concatenated.
pathSearch :: [FilePath] -> FilePath -> IO (Maybe FilePath)
pathSearch (a : r) file = do
    let candidate = a </> file
    fileExists <- doesFileExist candidate
    dirExists <- doesDirectoryExist candidate
    if fileExists || dirExists then
        return $ Just candidate
      else
        pathSearch r file
pathSearch [] _ = return Nothing

