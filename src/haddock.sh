#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

cabal configure
cabal haddock --executables --haddock-option=--ignore-all-exports
