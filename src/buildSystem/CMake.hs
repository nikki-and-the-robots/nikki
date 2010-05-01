
module CMake where


import Control.Monad

import System.FilePath
import System.Directory

import Utils

import Build


cmakeTargets :: String -> FilePath -> [Target]
cmakeTargets postfix path = [configure, build]
  where
    configure = 
        Target ("configure_" ++ postfix) [] $ 
            withCurrentDirectory path $ do
                trySystem "mkdir -p dist"
                withCurrentDirectory "dist" $ do
                    makeFileExists <- doesFileExist "Makefile"
                    unless makeFileExists $
                        trySystem "cmake .."
    build =
        Target ("build_" ++ postfix) [configure] $
            withCurrentDirectory (path </> "dist") $ do
                trySystem "make -j3"
