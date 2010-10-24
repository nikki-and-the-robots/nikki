#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

rm -rfv cpp/dist
cabal clean
rm -rfv scripts/dist
rm -rfv testsuite/dist
