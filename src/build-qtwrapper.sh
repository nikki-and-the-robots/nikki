#!/bin/bash

set -o errexit

cd cpp
mkdir -p dist
cd dist
cmake ..
make
cd ../..
