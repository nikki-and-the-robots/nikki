#!/bin/bash

# to abort in case of errors

function error {
echo aborted due to an error
exit 1
}
trap error ERR

echo ========================================================================
echo This is a script to facilitate deployment.
echo It tries to copy all needed files into a directory called "nikki",
echo and zips this folder.
echo You should specify the environment variable SFML_DIR.
echo SFML_DIR should contain the \(built\) svn version of sfml2.
echo ALso you need to specify the path to your Qt SDK in QT_DIR.
echo
echo ======================
echo TESTED ONLY ON WINDOWS
echo ======================
echo
echo ========================================================================
echo

if [ -z "$QT_DIR" ]
then
    echo QT_DIR not set. Please let QT_DIR point to your Qt SDK.
    exit 1
fi

if [ -z "$SFML_DIR" ]
then
    echo SFML_DIR not set. Please let SFML_DIR point to your SFML2 copy.
    exit 1
fi

echo ======================
echo cleaning...
echo ======================
if [ -d nikki ]
then
    rm -rfv nikki/*
fi

echo ======================
echo copying...
echo ======================
mkdir -p nikki
# sfml-dlls
cp -v $SFML_DIR/CSFML/lib/mingw/csfml-audio.dll nikki
cp -v $SFML_DIR/CSFML/lib/mingw/csfml-system.dll nikki
cp -v $SFML_DIR/extlibs/bin/openal32.dll nikki
cp -v $SFML_DIR/extlibs/bin/libsndfile-1.dll nikki
# qt-dlls
cp -v $QT_DIR/qt/bin/QtCore4.dll nikki
cp -v $QT_DIR/qt/bin/QtGui4.dll nikki
cp -v $QT_DIR/qt/bin/QtOpenGL4.dll nikki
# mingw-dlls
cp -v /mingw/bin/mingwm10.dll nikki
cp -v /mingw/bin/libgcc_s_dw2-1.dll nikki
# main binary
cp -v dist/build/nikki/nikki.exe nikki
# data
echo copying data...
cp -r ../data nikki
# and levels
echo copying levels...
cp -r ../levels nikki

# echo ======================
# echo zipping...
# echo ======================

# export ZIPFILE="nikki-$(date +%Y-%m-%d-%H-%M-%S).zip"
# zip -r $ZIPFILE nikki
# 
# # scp $ZIPFILE shahn@joyridelabs.de://var//joyride//darcs//alpha-binaries//win32//

# 
