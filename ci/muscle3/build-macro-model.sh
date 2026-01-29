#!/bin/bash

source ci/muscle3/setup-test-env.sh  $* || exit 1

cd ./tests/muscle3/macro
make all

