#!/bin/bash

ghci -iapplication -iapplication/dist/build/autogen -iqtRendering \
    -lQtOpenGL -lqtwrapper_so -LqtRendering/cpp/dist $@

