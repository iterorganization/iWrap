#!/bin/bash

declare -i tests_count=0
declare -i tests_failures=0
declare test_command
declare test_cases
declare failure_msg
declare -r project_root=$(realpath "$(dirname ${BASH_SOURCE})/..")
declare -r junit_report_template=$project_root/tests/misc/junit_report_template.in
declare -r junit_report_testcase_template=$project_root/tests/misc/junit_report_testcase_template.in
declare -r report_destination=$project_root/tests/reports/report.xml


# Exit shell on error
set -e

# Look for a tests artifacts with exit codes values
for report_dir_path in $project_root/tests/reports/**/; do

  echo `basename $report_dir_path`
  report_dir_name=$(basename $report_dir_path)

    tests_count=$tests_count+1

    test_command=$(echo ${report_dir_path}/exit_code.txt | cut -d'_' -f 2)

    if [ $(<${report_dir_path}/exit_code.txt) -ne 0 ]; then
        tests_failures=$tests_failures+1
        failure_msg="<failure message=\"$(cat ${report_dir_path}/stderr.txt | tr '\n' ' ' | tr "\"" "\'" | tr '<>' '.')\"/>"
    fi

    test_cases=$(echo $(<$junit_report_testcase_template) |
                (sed -e "s;__NAME__;${report_dir_name};g;" \
                     -e "s;__FAILURE_MSG__;$failure_msg;"))$test_cases

    failure_msg=""
done

echo -e "\n\t\033[1;33mGENERATING A TEST REPORT\033[0m\n"
sed -e "s;__ERRORS__;0;" \
    -e "s;__FAILURES__;$tests_failures;" \
    -e "s;__NAME__;$TEST_DIR_NAME;" \
    -e "s;__TESTS__;$tests_count;" \
    -e "s;__TIME__;$(date +'%Y-%m-%dT%H:%M:%S.%N');" \
    -e "s;__TEST_CASES__;$test_cases;" \
    $junit_report_template > $report_destination

echo -e "\t\033[0;33mReport saved in:\n\t\033[1;35m ${report_destination}\033[0m\n"