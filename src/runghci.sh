#!/bin/bash

cd ..
export nikki_datadir=$(pwd)/data
cd src
# this is a workaround for ghci-bug #5289
mkdir tempLibs
cd tempLibs
ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6 libstdc++.so
export LD_LIBRARY_PATH=$(pwd)
cd ..
ghci -i. -idist/build/autogen -itestsuite \
    -lQtOpenGL -lqtwrapper_so -Lcpp/dist \
    -L/usr/lib/x86_64-linux-gnu/gcc/x86_64-linux-gnu/4.5/ \
    $@
rm -rf tempLibs
