#!/bin/bash

declare -a test_dirs
declare -a test_exit_code

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
  echo -e "\n++++++++++++++++++++!!!--------------ONBAMBOO--------------!!!++++++++++++++++++++"
  # Source and run scripts with environment vars for CI server
  # set -e
  
  if [ -z "${TEST_DIR_NAME+x}" ]
  then
    >&2 echo -e "\n\n\tPLEASE DEFINE TEST_DIR_NAME!\n\n"
  else
    if [ -z "${TEST_COMMAND+x}" ]
    then
      >&2 echo -e "\n\n\tPLEASE DEFINE TEST_COMMAND!\n\n"
    else
      # CI bamboo server runs tests on root account...
      export OMPI_ALLOW_RUN_AS_ROOT=1
      export OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

      # Change file permissions before sourcing it (Bamboo issue)
      chmod a+x ./set-iter.sh
      # Source neccessary modules and environment variables to run
      . ./set-iter.sh

      # Navigate to the examples directory
      cd examples/

      # Navigate to a directory with example to be tested
      cd $TEST_DIR_NAME

      echo -e "***********:::---------Inside examples/${PWD##*/} directory------:::***********"

      echo -e "\n====================|||------Run make ${TEST_COMMAND} Tests------|||===================="

      echo -e "\n\t- - - - - - - - - - - Test in ${ACTOR_RUN_MODE} run mode - - - - - - - - - - -"

      make $TEST_COMMAND 1> make_stdout.txt 2> make_stderr.txt
      test_exit_code=$?
      # Log test's status
      test $test_exit_code -eq 0 && echo -e "\n\t\t\t\t   \033[0;32mPASS\033[0m" || >&2 echo -e "\n\t\t\t\t   \033[0;31mFAIL\033[0m"
      # Log test's stdoutput
      echo -e "\n  \033[0;35mOUTPUT:\033[0m\n" && cat make_stdout.txt
      # Log test's stderror
      test $test_exit_code -eq 0 || (echo -e "   \033[0;31mERROR:\033[0m\n" && >&2 cat make_stderr.txt)

      echo -e "\n====================|||-----Finished make ${TEST_COMMAND} Tests-----|||====================\n"

      # Exit immediately on non zero exit-code (Explicit)
      test $test_exit_code -eq 0 || exit $test_exit_code
    fi
  fi
fi
