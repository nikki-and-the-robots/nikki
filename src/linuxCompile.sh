#!/bin/bash

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
cmake ..
make
cd ../..

cabal configure $@
cabal build
