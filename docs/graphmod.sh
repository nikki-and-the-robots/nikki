#!/bin/bash

# script to visualize the module graph. graphmod (> 1.1.3 ???) is needed.
# graphmod outputs in dot format (so graphviz is probably needed).
# dot can create other formats than pdf.

graphmod ../src/application/Main.hs \
    -i ../src/application -i ../src/common -i ../src/qtRendering \
    -r Utils -r Control -r Physics -r Data -r Graphics \
    \
    \
    -r Editor -r Game -r Top -r Base \
    \
    | dot -Tpdf -o mods.pdf


