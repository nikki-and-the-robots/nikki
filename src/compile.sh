#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo This is a simple script, trying to make it easy to compile Nikki. Look at README for more details.
echo This is tested on linux and on windows using msys
echo You can create a file called "ghc_options" that contains options that will be passed to any calls of ghc.

if [ -f ghc_options ]
then
    GHC_OPTIONS=$(cat ghc_options)
else
    GHC_OPTIONS=""
fi

cd buildSystem
if [ -d dist ]
then
    echo
else
    cabal configure --ghc-options="$GHC_OPTIONS"
fi
cabal build
# ghc --make -i../common Main.hs -o build $GHC_OPTIONS
cd ..

BUILD_COMMAND="buildSystem/dist/build/build/build build_application"
if (which hate)
then
    $BUILD_COMMAND 2>&1 | hate application
else
    $BUILD_COMMAND
fi

