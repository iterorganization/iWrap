#!/bin/sh
# Task: Setup Environment and Run Pylint code check. Publish report artifacts with python modules and imas env modules.
set -e
my_dir=$(dirname $0)
envs_dir=$(dirname $0)/../envs/iter-bamboo

. $envs_dir/00_load_imas_env.sh
. $envs_dir/10_python_set_env.sh
. $envs_dir/03_report_module_list.sh