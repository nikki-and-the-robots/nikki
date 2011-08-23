#!/bin/bash

graphmod Main.hs \
    -R Data -R Distribution -R Graphics -R Control -R Physics -R Text -r Utils \
    -r Version -R Profiling -R Legacy \
    -c Base.Renderable -R Sorts \
    | dot -Tpdf -o moduleGraph.pdf
