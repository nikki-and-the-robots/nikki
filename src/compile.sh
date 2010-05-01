#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo This is a simple script, trying to make it easy to compile Nikki. Look at README for more details.

ghc --make -icommon -ibuildSystem build.hs
./build build_application

