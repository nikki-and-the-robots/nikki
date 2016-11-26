#!/usr/bin/env bash

set -eux

./build-qtwrapper.sh
hpack
stack test --only-dependencies
stack exec -- ghci test/Spec.hs
