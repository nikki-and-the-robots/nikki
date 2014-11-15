#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

rm -rfv cpp/dist
rm -rfv dist
rm -rfv scripts/dist
rm -rfv testsuite/dist
