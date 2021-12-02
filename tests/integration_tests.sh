#!/bin/bash

declare -a test_dirs

test_dirs+=("cp2ds")
test_dirs+=("cp2ds_cpp")
test_dirs+=("level2")
test_dirs+=("level2_cpp")
test_dirs+=("cp2ds-mpi")
test_dirs+=("cp2ds-mpi_cpp")
test_dirs+=("code_lifecycle")
test_dirs+=("code_lifecycle_cpp")

if [ -z "${ONBAMBOO+x}" ]
then
  cd ../examples

  for test_dir in ${test_dirs[@]}
  do
        echo ==========================================================================================
        echo ===   TESTING: $test_dir
        echo ==========================================================================================
        cd $test_dir
        echo - - - - - - - Building native code - - - - - - -
        make native > log.txt
        echo - - - - - - - - Actor generation - - - - - - - -
        make actor > log.txt
        echo - - - - - - - - Workflow test \(actor in NORMAL run mode\) - - - - - - - -
        make wf-run > log.txt
        echo - - - - - - - - Workflow test \(actor in STANDALONE run mode\) - - - - - - - -
        ACTOR_RUN_MODE='STANDALONE' make wf-run #>> log.txt
        cd ..
        echo ==========================================================================================
  done

  cd ..
  exit 0

else
  echo "++++++++++++++++++++!!!--------------ONBAMBOO--------------!!!++++++++++++++++++++"
  # Source and run scripts with environment vars for CI server
  set -e

  # Turn the bash array into a string
  TEST_DIRS=$( IFS=:; printf '%s' "${test_dirs[*]}" )
  export TEST_DIRS

  # CI bamboo server runs tests on root account...
  export OMPI_ALLOW_RUN_AS_ROOT=1
  export OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1


  chmod a+x ./set-iter.sh
  . ./set-iter.sh

  cd tests/

  # Run Pytests
  echo "====================|--------------Run Integration Tests--------------|===================="
  # Runs python pytest and logs results to junit xml file
  python -m pytest test_cases/test_integration/

fi
