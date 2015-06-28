#! /bin/bash

cabal install --only-dependencies --enable-tests
cabal test --test-options='--quickcheck-max-size 30 -j 4'
