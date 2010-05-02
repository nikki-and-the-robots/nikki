#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

./compile.sh

cd ..
src/application/dist/build/natr/natr

