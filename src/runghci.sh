#!/bin/bash

ghci -iapplication -iapplication/dist/build/autogen -icommon -iqtRendering \
    -lQtOpenGL -lqtwrapper_so -LqtRendering/cpp/dist $@

