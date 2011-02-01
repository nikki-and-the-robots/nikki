#!/bin/bash

cd ..
export nikki_datadir=$(pwd)/data
cd src
ghci -i. -idist/build/autogen -itestsuite \
    -lQtOpenGL -lqtwrapper_so -Lcpp/dist \
    $@

