#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo This is a script that facilitates development and is NOT part of the normal build system.

cabal build 2>&1 | hate 

cd ../..
src/application/dist/build/nikki/nikki default

