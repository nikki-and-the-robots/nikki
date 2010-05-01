
module CMake where


import System.FilePath

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
                    trySystem "cmake .."
    build =
        Target ("build_" ++ postfix) [configure] $
            withCurrentDirectory (path </> "dist") $ do
                trySystem "make"
