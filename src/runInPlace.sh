#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo ========================================================================
echo This script recompiles the haskell bits and runs the program \(with the given level\).
echo It does not recompile the Qt parts. \(compile.sh does\)
echo It is meant for development.
echo See README.
echo ========================================================================
echo

cd application

if (which hate)
then
    cabal build 2>&1 | hate
else
    cabal build
fi

export nikki_datadir=../..
dist/build/nikki/nikki $1

