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

cabal configure
cabal haddock --executables
echo actually in application/dist/doc/html/nikki/nikki/index.html
