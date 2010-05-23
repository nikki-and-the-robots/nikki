

import System
import System.IO

import Build
import Cabal
import CMake


main = do
    mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout, stderr]

    [target] <- getArgs
    (makeTargetByName allTargets) target

allTargets :: [Target]
allTargets =
    application ++ qtRendering


application :: [Target]
application = map (addDependencies qtRendering) $ cabalTargets "application" "./application"

qtRendering = cmakeTargets "qtRendering" "./qtRendering/cpp"

