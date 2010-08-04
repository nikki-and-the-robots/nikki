#!/bin/bash

# script to visualize the module graph. graphmod (> 1.1.3 ???) is needed.
# graphmod outputs in dot format (so graphviz is probably needed).
# dot can create other formats than pdf.

graphmod ../src/application/Main.hs -q \
    -i ../src/application -i ../src/common -i ../src/qtRendering \
    -R Data -R Graphics -R Physics -r Utils -r Paths \
    \
    \
    -c Top -c Sorts -c Game -c Base -c Editor -c Object\
    \
    | dot -Tpdf -o mods.pdf


