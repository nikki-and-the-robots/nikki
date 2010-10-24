#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

# install cabal deps with
# cabal install --with-ar=ar --with-gcc=gcc --with-ld=ld --ghc-options=" -pgmc gcc -pgml gcc -pgma gcc -pgmwindres windres -pgmP \"gcc -E -undef -traditional\""

# building c++-part (qt-bindings)
cd cpp
mkdir -p dist
cd dist
# cmake -G "MSYS Makefiles" ..
# make
cd ../..

cabal configure --with-ar=ar --with-gcc=gcc --with-ld=ld --ghc-options=" -pgmc gcc -pgml gcc -pgma gcc -pgmwindres windres -pgmP \"gcc -E -undef -traditional\""
cabal build
