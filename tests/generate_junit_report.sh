#!/bin/bash

declare -i tests_count=0
declare -i tests_failures=0
declare test_command
declare test_cases
declare failure_msg
declare -r project_root=$(pwd)
declare -r junit_report_template=$project_root/tests/test_data/junit_report_template.in
declare -r junit_report_testcase_template=$project_root/tests/test_data/junit_report_testcase_template.in
declare -r report_destination=$project_root/reports/report.xml


# Exit shell on error
set -e

# Change file permissions before sourcing it (Bamboo issue)
#chmod a+x ./set-iter.sh
# Source neccessary modules and environment variables to run
#. ./set-iter.sh

# Look for a tests artifacts with exit codes values
for file in $project_root/reports/*_exit_code.txt; do
    tests_count=$tests_count+1

    test_command=$(echo $file | cut -d'_' -f 2)

    if [ $(<$file) -ne 0 ]; then
        tests_failures=$tests_failures+1
        failure_msg="<failure message='$(cat ${project_root}/reports/make_${test_command}_stderr.txt)'></failure>"
    fi

    test_cases_previous=$test_cases
    test_cases=$(echo $(<$junit_report_testcase_template) |
                (sed -e "s;__NAME__;$TEST_DIR_NAME;g;" \
                     -e "s;__TESTCOMMAND__;$test_command;" \
                     -e "s;__FAILURE_MSG__;$failure_msg;"))
    test_cases=$test_cases$test_cases_previous
    
    failure_msg=""
done

echo -e "test cases outcome:\n\t$test_cases"

echo -e "\n\t\033[1;33mGENERATING A TEST REPORT\033[0m\n"
sed -e "s;__ERRORS__;0;" \
    -e "s;__FAILURES__;$tests_failures;" \
    -e "s;__NAME__;$TEST_DIR_NAME;" \
    -e "s;__TESTS__;$tests_count;" \
    -e "s;__TIME__;$(date +'%Y-%m-%dT%H:%M:%S.%N');" \
    -e "s;__TEST_CASES__;$test_cases;" \
    $junit_report_template > $report_destination

echo -e "\t\033[0;33mReport saved in:\n\t\033[1;35m ${report_destination}\033[0m\n"