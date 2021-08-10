#!/bin/sh --login
# Task: Setup Environment and Run Examples.
set -e

export PATH=${PWD}/bin:${PATH}
export PATH=${PWD}:${PATH}

example_dir=`pwd`/examples
envs_dir=`pwd`/envs/iter-bamboo
chmod a+x $envs_dir/00_load_imas_env.sh
. $envs_dir/00_load_imas_env.sh
. $envs_dir/10_python_set_env.sh

cd $example_dir/cp2ds
echo "####### MAKE NATIVE ##########"
make native
echo "####### MAKE ACTOR ##########"
make actor
echo "####### MAKE WF-RUN ##########"
make wf-run