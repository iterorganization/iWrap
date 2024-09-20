#!/bin/sh --login
# Task: Setup Environment and report available modules. Save output to artifact.
set -e

my_dir=`pwd`/envs/iter-bamboo

chmod a+x $my_dir/00_load_imas_env.sh

. $my_dir/00_load_imas_env.sh
. $my_dir/02_report_module_avail.sh