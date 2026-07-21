#!/bin/sh --login
# Task: Setup Environment and Run Pylint code check. Publish report artifacts with python modules and imas env modules.
set -e

source ci/muscle3/setup-test-env.sh  "$@" || exit 1
echo "--------------Module load  Pylint--------------"
module load Pylint/3.2.5-GCCcore-13.2.0
python -m pylint -E ./iwrap_plugins > pylint.log


