#!/bin/bash
# Prints logs for the first _dd entry in func test
testdir=$(ls _dd | head -n 1)
service=${1:-strata_fullnode}
testenv=${2:-checkpoint}
cat "_dd/$testdir/$testenv/$service/service.log"
