#!/bin/sh
mkdir -p .shake
ghc --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=.shake -o .shake/build && .shake/build "$@"
