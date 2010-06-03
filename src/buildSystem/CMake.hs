
module CMake where


import Control.Monad

import System.FilePath
import System.Directory
import System.Info

import Utils

import Build


cmakeTargets :: String -> FilePath -> [Target]
cmakeTargets postfix path = [configure, build]
  where
    configure = 
        Target ("configure_" ++ postfix) [] $ 
            withCurrentDirectory path $ do
                createDirectoryIfMissing True "dist"
                withCurrentDirectory "dist" $ do
                    makeFileExists <- doesFileExist "Makefile"
                    unless makeFileExists $
                        trySystem cmakeCommand
    build =
        Target ("build_" ++ postfix) [configure] $
            withCurrentDirectory (path </> "dist") $ do
                trySystem "make -j3"

cmakeCommand =
    case os of
         "linux" -> "cmake .."
         "mingw32" -> "cmake -G \"MSYS Makefiles\" .."
         x -> error ("unsupported platform: " ++ os)
                 