#!/bin/bash

# to abort in case of errors
function error {
    local exit_status=${1:-$?}
    echo aborted due to an error \($exit_status\)
    exit $exit_status
}
trap error ERR

# setting LD_LIBRARY_PATH
deployDir=$(dirname "$(readlink -f $0)")
export LD_LIBRARY_PATH=$deployDir:$LD_LIBRARY_PATH

"$deployDir/restarter" $@
