#!/usr/bin/env bash

set -eux

./build-qtwrapper.sh
stack exec -- ghci test/testsuite.hs
