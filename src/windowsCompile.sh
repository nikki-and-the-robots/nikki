#!/bin/bash

# This script attempts to build the game on windows.
# It invokes cabal configure with a lot of options to convince it
# to use the build toolchain in the $PATH and not the one shipped with ghc.
# (Which should be the toolchain shipped with Qt in our case.)
#
# cabal dependencies must be installed using the same compiler, e.g.:
# cabal install --with-ar=ar --with-gcc=gcc --with-ld=ld --ghc-options=" -pgmc gcc -pgml gcc -pgma gcc -pgmwindres windres -pgmP \"gcc -E -undef -traditional\"" package

echo This script will pass any command line options to "cabal configure".

# to abort in case of errors
function error {
echo aborted due to an error
exit 1
}
trap error ERR

# building c++-part (qt-bindings)
cd cpp
mkdir -p dist
cd dist
cmake -G "MSYS Makefiles" ..
make
cd ../..

cabal install --with-ar=ar --with-gcc=gcc --with-ld=ld --ghc-options=" -pgmc gcc -pgml gcc -pgma gcc -pgmwindres windres -pgmP \"gcc -E -undef -traditional\"" $@
