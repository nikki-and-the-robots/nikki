#!/bin/bash

# script to visualize the module graph. graphmod (> 1.1.3 ???) is needed.
# graphmod outputs in dot format (so graphviz is probably needed).
# dot can create other formats than pdf and okular isn't necessarily needed, of course.

graphmod ../src/application/Main.hs \
    -i ../src/application -i ../src/common -i ../src/qtRendering \
    -r Utils -r Control -r Physics -r Data -r Graphics \
    \
    \
    -c Editor -c Game -c Objects -c Top -c Base \
    \
    | dot -Tpdf -o mods.pdf


