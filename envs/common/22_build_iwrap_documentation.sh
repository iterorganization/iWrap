#!/bin/sh --login
envs_dir=`pwd`/envs/
. `pwd`/set-iter.sh
. $envs_dir/common/13_python_create_env.sh

./docs/scripts/create_db
echo "-----------------IMAS DB created------------"

. `pwd`/venv/bin/activate
# . $envs_dir/21_install_python_packages_for_tutorial.sh

python -m pip install --ignore-installed ".[docs]"

cd docs/

echo "--------Building Jupyter-Book-------------"
jupyter-book build .
