#!/bin/bash

# Script to visualize the module graph. graphmod (> 1.2.3) is probably needed.
# Add graphmod options to tweak the graph to your needs.

# graphmod outputs in dot format (so graphviz is probably needed).
# dot can create other formats than pdf.


graphmod ../src/Main.hs -q -i ../src/ \
    -R Data -R Graphics -R Physics -r Utils \
    | dot -Tpdf -o mods.pdf


