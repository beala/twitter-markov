#! /bin/bash

cabal test --test-options='--quickcheck-max-size 30 -j 4 -t 1s'