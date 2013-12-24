#!/bin/bash
set -e # exit on errors
set -x # echo each line

# Standard testing
wget https://raw.github.com/ndmitchell/neil/master/travis.sh -O - --no-check-certificate --no-cache --quiet | sh

# This script benchmarks time to build Ninja with both Shake and Ninja

# Install shake
cabal install

# Grab ninja
git clone https://github.com/martine/ninja
(cd ninja && ./bootstrap.py)
mkdir bin
cp ninja/ninja bin/ninja
export PATH=$PATH:`pwd`/bin

cd ninja
function timed
{
    local START=`date +%s%N`
    time $1
    local END=`date +%s%N`
    RET=$(((END - START) / 1000000)) # in milliseconds
    echo Took ${RET}ms
}

echo Run Ninja
ninja -t clean
timed "time ninja -j3 -d stats"
NINJA_FULL=$RET
timed "time ninja -j3 -d stats"
NINJA_ZERO=$RET

echo Run Shake
ninja -t clean
timed "shake -j3 --quiet --timings"
SHAKE_FULL=$RET
timed "shake -j3 --quiet --timings"
SHAKE_ZERO=$RET

ls -l .shake* build/.ninja*

echo Ninja was $NINJA_FULL then $NINJA_ZERO
echo Shake was $SHAKE_FULL then $SHAKE_ZERO

if (( NINJA_FULL < SHAKE_FULL )); then
    echo ERROR: Ninja build was faster than Shake
    exit 1
fi

if (( NINJA_ZERO + 100 < SHAKE_ZERO )); then
    echo ERROR: Ninja zero build was more than 0.1s faster than Shake
    exit 2
fi
