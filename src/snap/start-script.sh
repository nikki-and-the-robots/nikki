#!/usr/bin/env bash

set -o errexit

BIN_DIR=$(dirname $0)
$BIN_DIR/../lib/nikki/nikki.sh
