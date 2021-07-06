#!/bin/sh --login
# Task: Setup Environment and Run Pylint code check. Publish report artifacts with python modules and imas env modules.
set -e
my_dir=$(dirname $0)
wrk_dir=$(dirname $0)/..
envs_dir=`pwd`/envs/iter-bamboo
echo "PWD : `pwd`"
echo "envs_dir : `$envs_dir`"
chmod a+x $envs_dir/00_load_imas_env.sh
.$envs_dir/00_load_imas_env.sh
. $envs_dir/10_python_set_env.sh
. $envs_dir/03_report_module_list.sh

# Run pylint code check
echo "~~~~~====================PYLINT CODE CHECK====================~~~~~"
python -m pylint -E --output-format=pylint_junit.JUnitReporter iwrap > pylint.xml