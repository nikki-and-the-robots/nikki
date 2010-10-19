#!/bin/bash

cd ..
export nikki_datadir=$(pwd)
cd src
ghci -iapplication -iapplication/dist/build/autogen -iqtRendering -itestsuite \
    -lQtOpenGL -lqtwrapper_so -LqtRendering/cpp/dist \
    -lcsfml-system -lcsfml-audio \
    $@

