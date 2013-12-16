#!/bin/bash
# This script benchmarks time to build Ninja with both Shake and Ninja
set -e # exit on errors
set -x # echo each line

# Install shake
cabal install

# Grab ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
mkdir bin
cp ninja/ninja bin/ninja
export PATH=$PATH:`pwd`/bin

cd ninja
echo Run Ninja
ninja -t clean
time ninja -j3
time ninja -j3

echo Run Shake
ninja -t clean
time shake -j3
time shake -j3
