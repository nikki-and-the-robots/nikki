#!/bin/bash

cd ..
export nikki_datadir=$(pwd)
cd src
ghci -hide-package monads-tf \
    -i. -idist/build/autogen -itestsuite \
    -lQtOpenGL -lqtwrapper_so -Lcpp/dist \
    -lcsfml-system -lcsfml-audio \
    $@

