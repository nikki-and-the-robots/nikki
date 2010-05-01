

import System

import Build
import Cabal
import CMake


main = do
    [target] <- getArgs
    (makeTargetByName allTargets) target

allTargets :: [Target]
allTargets =
    application ++ qtRendering


application :: [Target]
application = map (addDependencies qtRendering) $ cabalTargets "application" "./application"

qtRendering = cmakeTargets "qtRendering" "./qtRendering/cpp"

