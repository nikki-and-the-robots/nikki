#!/bin/bash

cd ..
export nikki_datadir=$(pwd)
cd src
ghci -i. -idist/build/autogen -itestsuite \
    -lQtOpenGL -lqtwrapper_so -Lcpp/dist \
    -lcsfml-system -lcsfml-audio \
    $@

