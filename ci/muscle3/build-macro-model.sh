#!/bin/bash

source ci/muscle3/setup-test-env.sh  $* || exit 1

cd ./tests/macro
make all

