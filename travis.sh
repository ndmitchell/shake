#!/bin/bash -e
rm -f dist/*.tar.gz
rm -rf dist/snapshot
cabal configure --flag=testprog --ghc-options=-Werror
cabal sdist
cd dist
mkdir snapshot
tar -xf *.tar.gz -C snapshot
cd snapshot
mv shake-* shake # predictable name
cd shake
cabal configure --flag=testprog --ghc-options=-Werror
cabal build
shake_datadir=. dist/build/shake-test/shake-test test
shake_datadir=. dist/build/shake-test/shake-test random test 3m
