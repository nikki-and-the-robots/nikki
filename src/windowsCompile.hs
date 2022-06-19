#!/usr/bin/env stack
{- stack script --resolver=lts-3.22 -}

-- This script attempts to build the game on windows.
-- It invokes cabal configure with a lot of options to convince it
-- to use the build toolchain in the $PATH and not the one shipped with ghc.
-- (Which should be the toolchain shipped with Qt in our case.)
-- -- cabal dependencies must be installed using the same compiler, e.g.:
-- cabal install --with-ar=ar --with-gcc=gcc --with-ld=ld --ghc-options=" -pgmc gcc -pgml gcc -pgma gcc -pgmwindres windres -pgmP \"gcc -E -undef -traditional\"" package

import Development.Shake

main :: IO ()
main = do
  -- unit $ cmd "mkdir -p cpp/dist"
  unit $ cmd "choco install cmake"
  unit $ cmd "cmake --version"
  unit $ cmd (Cwd "cpp/dist") "cmake -G" ["'MSYS Makefiles'"] ".."
--
-- # building c++-part (qt-bindings)
-- cd cpp
-- mkdir -p dist
-- cd dist
-- cmake -G "MSYS Makefiles" ..
-- make
-- cd ../..
--
-- export C_INCLUDE_PATH=/usr/local/include
-- export LIBRARY_PATH=/usr/local/lib
--
-- cabal install $@
