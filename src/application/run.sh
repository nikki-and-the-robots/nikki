#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo This is a script that facilitates development and is NOT part of the normal build system.

if (which hate)
then 
    cabal build 2>&1 | hate
else
    cabal build
fi

cd ../..
src/application/dist/build/nikki/nikki default

