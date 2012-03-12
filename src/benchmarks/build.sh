#!/bin/bash

mkdir -p build

ghc --make  -O2 -i.. searchGlyphs.hs -outputdir build \
-lqtwrapper -L../cpp/dist \
-lQtCore -lQtGui -lQtOpenGL
