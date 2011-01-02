#!/bin/bash

# to abort in case of errors
function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo running in place \(without auto-updating\)

dist/build/core/core --runinplace $@ +RTS -C
