#!/bin/bash
# This script benchmarks time to build Ninja with both Shake and Ninja
set -e # exit on errors
set -x # echo each line

# Grab ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

cd ninja
cp ninja ninji # So cleaning doesn't wipe the ninja binary
echo Run Ninja
ninji -t clean
date +%H:%M:%S.%N
ninji -j3
date +%H:%M:%S.%N
ninji -j3
date +%H:%M:%S.%N

echo Run Shake
ninji -t clean
date +%H:%M:%S.%N
shake -j3
date +%H:%M:%S.%N
shake -j3
date +%H:%M:%S.%N
