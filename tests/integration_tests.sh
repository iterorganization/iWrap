#!/bin/bash


declare -a test_dirs

test_dirs+=("cp2ds")
test_dirs+=("cp2ds_cpp")
test_dirs+=("level2")
test_dirs+=("level2_cpp")
test_dirs+=("cp2ds-mpi")
test_dirs+=("cp2ds-mpi_cpp")


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
