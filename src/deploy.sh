#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo This is a script to facilitate deployment.
echo It tries to compile Nikki and copy all needed files into a directory called "nikki",
echo and zips this folder.
echo You should specify the environment variable SFML_DIR.
echo SFML_DIR should contain the \(built\) svn version of sfml2.
echo
echo ======================
echo TESTED ONLY ON WINDOWS
echo ======================
echo

echo ======================
echo compiling...
echo ======================
./compile.sh

echo ======================
echo cleaning...
echo ======================
if [ -d nikki ]
then
    rm -rf nikki/*
fi

echo ======================
echo copying...
echo ======================
mkdir -p nikki
# main binary
cp -v application/dist/build/nikki/nikki.exe nikki
# needed dlls
cp -v $SFML_DIR/CSFML/lib/mingw/csfml-audio.dll nikki
cp -v $SFML_DIR/CSFML/lib/mingw/csfml-system.dll nikki
cp -v $SFML_DIR/extlibs/bin/openal32.dll nikki
cp -v $SFML_DIR/extlibs/bin/libsndfile-1.dll nikki
# data
cp -rv ../data nikki
# and levels
cp -rv ../levels nikki

