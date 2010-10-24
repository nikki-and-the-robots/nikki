#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo ========================================================================
echo This is a simple script, trying to make it easy to compile Nikki. Look at README for more details.
echo This is tested on linux and on windows using msys
echo ========================================================================
echo
echo compiling:

# building c++-part (qt-bindings)
cd cpp
mkdir -p dist
cd dist
cmake ..
make
cd ../..

cabal configure
cabal build
