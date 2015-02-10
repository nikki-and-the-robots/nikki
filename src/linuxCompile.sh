#!/bin/bash

set -o errexit

# building c++-part (qt-bindings)
cd cpp
mkdir -p dist
cd dist
cmake ..
make
cd ../..

cabal sandbox init
cabal install --only-dependencies $@
cabal configure $@
cabal build
