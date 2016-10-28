#!/bin/bash

set -o errexit

# building c++-part (qt-bindings)
cd cpp
mkdir -p dist
cd dist
cmake ..
make
cd ../..

stack build
