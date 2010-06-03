
import Control.Applicative

import System
import System.IO
import System.Directory

import Build
import Cabal
import CMake


main = do
    mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout, stderr]

    [target] <- getArgs
    allTargets_ <- allTargets
    (makeTargetByName allTargets_) target

allTargets :: IO [Target]
allTargets = do
    application_ <- application
    return (application_ ++ qtRendering)


application :: IO [Target]
application = do
    ghc_options <- readCabalOptions
    return $ map (addDependencies qtRendering) $
        cabalTargets "application" "./application" ghc_options
        
readCabalOptions :: IO CabalOptions
readCabalOptions =
    CabalOptions <$> readFileIfExists "cabal_options" <*> readFileIfExists "ghc_options"

-- | returns the empty string, if the file doesn't exist.
readFileIfExists :: FilePath -> IO String
readFileIfExists file = do
    exists <- doesFileExist file
    if exists then
       readFile file
     else
       return ""

qtRendering = cmakeTargets "qtRendering" "./qtRendering/cpp"

