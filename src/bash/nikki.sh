#!/bin/bash

# to abort in case of errors
function error {
echo aborted due to an error
exit 1
}
trap error ERR

# setting LD_LIBRARY_PATH
deployDir=$(dirname $(readlink -f $0))
export LD_LIBRARY_PATH=$deployDir:$LD_LIBRARY_PATH

$deployDir/restarter $@
