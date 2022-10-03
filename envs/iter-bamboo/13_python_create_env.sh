#!/bin/sh --login
set -e

envs_dir=`pwd`/envs/iter-bamboo
chmod a+x $envs_dir/00_load_imas_env.sh
. $envs_dir/00_load_imas_env.sh
. $envs_dir/10_python_set_env.sh
. $envs_dir/11_python_build_env.sh
. `pwd`/venv/bin/activate
. $envs_dir/12_python_env_install.sh

export python_path=`which python`
echo -e Using python from env: $python_path

