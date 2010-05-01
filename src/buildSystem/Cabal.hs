
module Cabal where


import Utils

import Build


-- | creates targets for cabal builds
cabalTargets :: String -> FilePath -> [Target]
cabalTargets postfix path =
    [configure, build]
  where
    configure =
        Target confName [] $
            withCurrentDirectory path $ do
                trySystem "cabal configure"
    build =
        Target buildName [configure] $
            withCurrentDirectory path $ do
                trySystem "cabal build"

    confName = "configure_" ++ postfix
    buildName = "build_" ++ postfix


