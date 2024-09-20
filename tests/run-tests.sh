#!/bin/bash

declare -i test_exit_code

# CI bamboo server runs tests on root account...
export OMPI_ALLOW_RUN_AS_ROOT=1
export OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1


PASSED='\033[1;32m- Passed.\033[0m'
FAILED='\033[1;31m- Failed.\033[1;33m See log.txt.\033[0m Continuing...'

ALL_PARAMETER_FORMATS=("none" "legacy-xml" "xml" "namelist" "yaml" "json")
ALL_CODE_LANGUAGES=("dummy" "fortran" "cpp" "java")
ALL_RUN_MODES=("normal" "standalone")
ALL_VENDORS=("gcc" "intel")
ALL_SITES=("iter" "gw")
ALL_AL_VERSIONS=("4" "5")

declare -A LANGUAGE_FORMATS_DICT
LANGUAGE_FORMATS_DICT["fortran"]="legacy-xml xml json namelist none"
LANGUAGE_FORMATS_DICT["cpp"]="legacy-xml xml json none"
LANGUAGE_FORMATS_DICT["java"]="legacy-xml xml json none"
LANGUAGE_FORMATS_DICT["dummy"]="none"

########################################################################################################################
#                                                  SCRIPT FUNCTIONS                                                    #
########################################################################################################################

##########################################################################
#                             PRINT HELP                                 #
##########################################################################
print_help() {
    echo -e "Usage:"
    echo -e "\t $0 [--language|-l <language>] [--test-case|-t <test name>] [--compiler|-c <vendor> ] [--run-mode |-m <mode> ] [--help|-h]"
    echo
    echo -e "optional arguments:"
    echo -e "\t -l, --language <language> \t Test given language only. Available values: '${ALL_CODE_LANGUAGES[*]}'"
    echo -e "\t -t, --test-case <test name> \t Run specified test case. Default: all values"
    echo -e "\t -c, --compiler <vendor> \t Specifies compiler vendor. Available values: '${ALL_VENDORS[*]}'. Default: 'gcc'"
    echo -e "\t -m, --run-mode <mode>   \t Defines actor run mode. Available values: '${ALL_RUN_MODES[*]}'. Default: all values "
    echo -e "\t -s, --test-site <site>   \t Site where tests are run. Available values: '${ALL_SITES[*]}'. Default: 'iter'"
    echo -e "\t -a, --al-version <version>   \t IMAS Access Layer version. Available values: '${ALL_AL_VERSIONS[*]}'. Default: '5'"
    echo -e "\t -h, --help             \t show this help message and exit"
}

##########################################################################
#                  PRINT CURRENT TEST CONFIGURATION                      #
##########################################################################
print_configuration() {
    echo -e "------------------------------------------------------------"
    echo -e "Test configuration:"
    echo -e "\t Code language(s): '${tested_languages[*]}'."
    echo -e "\t Parameters formats: '${tested_formats[*]}'."
    echo -e "\t Run mode(s): '${tested_run_modes[*]}'"
    echo -e "\t Test(s) to be run: ${test_cases[*]}"
    echo -e "------------------------------------------------------------"
    echo
}

##########################################################################
#                      TEST CASES DISCOVERY                              #
##########################################################################
get_all_test_cases() {
# Returns names of all directories containing file 'test.cfg'

   cd ${TEST_CASES_DIR}

   test_dirs=(*/test.cfg)

   test_cases=()
    for test_dir in "${test_dirs[@]}"
    do
      test_cases+=($(basename $(dirname ${test_dir})))
    done

    cd ${IWRAP_TESTS_DIR}
    echo ${test_cases[@]}

}

##########################################################################
#                      CLEAR TEST ENVIRONMENT                            #
##########################################################################
clear_test_environment() {
# Unsets variables set by `test.cfg`
  unset ACTOR_NAME
  unset TEST_SCRIPT
  unset CODE_NAME
  unset TEST_LANGUAGES
  unset TEST_PARAMETERS_FORMATS
}

##########################################################################
#                   RESOLVE PARAMETERS FORMAT                            #
##########################################################################
resolve_parameters_format () {
    # arg $1 - parameters format to be processed [xml, json, namelist, legacy-xml, none]
    if [ $1 = "xml" ] || [ $1 = "legacy-xml" ]; then
        export PARAMETERS_EXTENSION="xml"
        export SCHEMA_EXTENSION="xsd"
    elif [ $1 = "json" ]; then
        export PARAMETERS_EXTENSION="json"
        export SCHEMA_EXTENSION="json"
    elif [ $1 = "nml" ] || [ $1 = "namelist" ]; then
        export PARAMETERS_EXTENSION="nml"
        export SCHEMA_EXTENSION="json"
    elif [ $1 = "yaml" ]; then
        export PARAMETERS_EXTENSION="yaml"
        export SCHEMA_EXTENSION="json"
    elif [ $1 = "none" ]; then
        export PARAMETERS_EXTENSION=
        export SCHEMA_EXTENSION=
    else
        echo "ERROR WHEN RESOLVING PARAMETERS FORMAT: Unknown parameter type:" $1
    fi
}


##########################################################################
#                 PARSE COMMAND LINE ARGUMENTS                           #
##########################################################################
parse_command_line () {

    while [ $# -gt 0 ]; do
      case "$1" in
        --language|-l)
           tested_languages=($2)
           shift
          ;;

        --test_case|-t)
           test_cases=($2)

          # TODO: Check if dir exists
           shift
          ;;

        --compiler|-c)
             compiler_vendor=${2,,}
           shift
          ;;

        --run-mode|-m)
             tested_run_modes=($2)
           shift
          ;;

        --parameters-format|-f)
             tested_formats=($2)
           shift
          ;;

        --test-site|-s)
           current_site=${2,,}
           shift
          ;;

        --al-version|-a)
           al_version=${2}
           shift
          ;;

        --help|-h)
          print_help
          exit 0
          ;;
        *)
          echo "ERROR: Incorrect parameter: \"$1\"\n"
          print_help
          exit 1
      esac
      shift
    done
}

##########################################################################
#                            RUN TEST                                    #
##########################################################################
do_test() {
        echo "=========================================================================================="
        echo "==="
        echo "===    TESTING: ${test_case^^}"
        echo "==="
        echo "===    Code language:     ${code_language^^} "
        echo "===    Parameters format: ${parameters_format^^} "
        echo "===    Run mode:          ${run_mode^^}"
        echo "=========================================================================================="

        reports_dir=${TESTS_DIR}/reports/${test_case}
        if [[ "${code_language}" != "dummy" ]]; then
            reports_dir=${reports_dir}_${code_language}
        fi

        if [[ "${parameters_format}" != "none" ]]; then
            reports_dir=${reports_dir}_${parameters_format}
        fi

        reports_dir=${reports_dir}_${run_mode}
        mkdir -p ${reports_dir}
        # set PARAMETERS_EXTENSION and SCHEMA_EXTENSION variables
        resolve_parameters_format ${parameters_format}

        CI_RUN='true' \
        CODE_LANGUAGE=${code_language} \
        PARAMETERS_FORMAT=${parameters_format} \
        RUN_MODE=${run_mode} \
        make test > >(tee -a ${reports_dir}/stdout.txt) 2> >(tee -a ${reports_dir}/stderr.txt >&2)
        test_exit_code=$?

        # Summary of all failed tests
        if [ ${test_exit_code} -ne 0 ]; then
            echo "${test_case^^}: Language ${code_language} // Run mode: ${run_mode}  // Parameters format: ${parameters_format^^}" >> ${TESTS_DIR}/reports/failed_tests.log
        fi

        # Save exit code for a reporting purpose
        echo "${test_exit_code}" > ${reports_dir}/exit_code.txt

        # Log test's status
        test "${test_exit_code}" -eq "0" && echo -e ${PASSED} || echo -e ${FAILED}


        echo ==========================================================================================
}


########################################################################################################################
#                                                 SCRIPT STARTS HERE                                                   #
########################################################################################################################


TEST_CASES_DIR=${TESTS_DIR}/"test_cases"

# set default values of parameters
tested_languages=(${ALL_CODE_LANGUAGES[@]})
tested_run_modes=(${ALL_RUN_MODES[@]})
tested_formats=(${ALL_PARAMETER_FORMATS[@]})
compiler_vendor='gcc'
current_site='iter'
al_version='5'

# get command line settings
parse_command_line "$@"


source ../envs/set-env.sh ${current_site} ${compiler_vendor} ${al_version}
ret_val=$?
if [ ${ret_val} -ne 0 ]; then
    echo "Environment cannot be configured. Exiting...."
    exit 1
fi

export TEST_CASES_DIR=${TESTS_DIR}/test_cases
if [ -z ${test_cases} ]; then
    test_cases=($(get_all_test_cases))
fi

# print current test configuration
print_configuration

# cleaning up directory with test results
rm -rf ${TESTS_DIR}/reports


 cd ${TEST_CASES_DIR}
##########################################################################
#                  LOOP OVER RUN CODE LANGUAGES                          #
##########################################################################
for code_language in "${tested_languages[@]}"
do

    echo "=========================================================================================="
    echo "===   CODE LANGUAGE: ${code_language^^}"
    echo "=========================================================================================="

    ##########################################################################
    #                      LOOP OVER TEST CASES                              #
    ##########################################################################
    for test_case in "${test_cases[@]}"
    do
        # Navigate to a test directory
        cd ${test_case}

        # Unset test variables
        clear_test_environment

        # Read test configuration
        #cat ./test.cfg
        source ./test.cfg

        # If TEST_LANGUAGES is empty, set language to DUMMY
        if [[ -z "${TEST_LANGUAGES}" ]]; then
          TEST_LANGUAGES=("dummy")
        fi

        # Check if test case should be run for a given language
        if [[ ! "${TEST_LANGUAGES}" =~ "${code_language}" ]]; then
          echo -e "INFO: \t Test '${test_case^^}' for '${code_language}' skipped (the only language(s): '${TEST_LANGUAGES[*]}' specified for this testcase)"
          cd ${TEST_CASES_DIR}
          continue
        fi

        ##########################################################################
        #                LOOP OVER RUN PARAMETERS FORMATS                        #
        ##########################################################################
        for parameters_format in "${tested_formats[@]}"
        do

            if [[ -z "${TEST_PARAMETERS_FORMATS}" ]]; then
              TEST_PARAMETERS_FORMATS=("none")
            fi

          # Check if test case should be run for a given parameters format
          if [[ ! "${TEST_PARAMETERS_FORMATS}" =~ ${parameters_format} ]]; then
            echo -e "INFO: \t Test '${test_case^^}:${code_language}' for '${parameters_format}' skipped (the only format(s): '${TEST_PARAMETERS_FORMATS[*]}' specified for this testcase)"
            continue
          fi

          # Skip format if it is not supported by code_language
          if [[ " ${LANGUAGE_FORMATS_DICT[${code_language}]} " != *"$parameters_format"* ]]; then
            echo -e "INFO: \t Test '${test_case^^}:${code_language}' for '${parameters_format}' skipped ('${parameters_format}' is not supported by ${code_language})"
            continue
          fi

          ##########################################################################
          #                       LOOP OVER RUN MODES                              #
          ##########################################################################
          for run_mode in "${tested_run_modes[@]}"
          do
              #####################################
              #             RUN TEST              #
              #####################################
              do_test
              #####################################
          done # LOOP OVER RUN MODES

        done # LOOP OVER PARAMETERS FORMATS
        cd ${TEST_CASES_DIR}

    done  # LOOP OVER TEST CASES
done # LOOP OVER CODE LANGUAGES

exit 0
