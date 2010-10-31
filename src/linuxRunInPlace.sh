#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

cd ..
export nikki_datadir=$(pwd)/data
cd src
dist/build/nikki/nikki +RTS -C
