#!/bin/bash

declare -i tests_count=0
declare -i tests_failures=0
declare -r project_root=$(pwd)
declare -r junit_report_template=$project_root/tests/test_data/junit_report_template.in
declare -r report_destination=$project_root/reports/report.xml


# Exit shell on error
set -e

# Change file permissions before sourcing it (Bamboo issue)
chmod a+x ./set-iter.sh
# Source neccessary modules and environment variables to run
. ./set-iter.sh

# Look for a tests artifacts with exit codes values
for file in $project_root/reports/*_exit_code.txt; do
    tests_count=$tests_count+1
    
    if [ $(<$file) -ne 0 ]; then
        tests_failures=$tests_failures+1
    fi
done

echo -e "\n\t\033[1;33mGENERATING A TEST REPORT\033[0m\n"
sed -e "s;__ERRORS__;0;" \
    -e "s;__FAILURES__;$tests_failures;" \
    -e "s;__NAME__;TEST $TEST_DIR_NAME;" \
    -e "s;__TESTS__;$tests_count;" \
    -e "s;__TIME__;$(date +"%Y-%m-%dT%H:%M:%S.%N");" \
    $junit_report_template > $report_destination

echo -e "\t\033[0;33mReport saved in:\n\t\033[1;35m ${report_destination}\033[0m\n"