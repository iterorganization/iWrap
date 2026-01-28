#!/bin/bash

source ci/muscle3/setup-test-env.sh  $* || exit 1

cd $TEST_DIR

echo "- - - - - TEST CASE: " $TEST_DIR " - - - - - - - - - - - - "
make test

