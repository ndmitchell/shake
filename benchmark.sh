#!/bin/bash
# This script benchmarks time to build Ninja with both Shake and Ninja
set -e # exit on errors
set -x # echo each line

# Grab ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
export PATH=$PATH:`pwd`/ninja

ls /usr/bin
(cd ninja && ninja -t clean && date +%H:%M:%S.%N && ninja -j3 && date +%H:%M:%S.%N && ninja -j3 && date +%H:%M:%S.%N)
(cd ninja && ninja -t clean && date +%H:%M:%S.%N && shake -j3 && date +%H:%M:%S.%N && shake -j3 && date +%H:%M:%S.%N)
