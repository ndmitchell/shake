#!/bin/bash
# This script benchmarks time to build Ninja with both Shake and Ninja
set -e # exit on errors
set -x # echo each line

# Grab ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

cd ninja
echo Run Ninja
ninja -t clean
date +%H:%M:%S.%N
ninja -j3
date +%H:%M:%S.%N
ninja -j3
date +%H:%M:%S.%N

echo Run Shake
ninja -t clean
date +%H:%M:%S.%N
shake -j3
date +%H:%M:%S.%N
shake -j3
date +%H:%M:%S.%N
