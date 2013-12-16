#!/bin/bash
# This script benchmarks time to build Ninja with both Shake and Ninja
set -e # exit on errors
set -x # echo each line

# Grab ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

(cd ninja && ninja -t clean && time ninja -j3 && time ninja -j3)
(cd ninja && ninja -t clean && time shake -j3 && time shake -j3)
