#!/bin/sh --login
envs_dir=`pwd`/envs/
. `pwd`/set-iter.sh
. $envs_dir/common/13_python_create_env.sh

./docs/scripts/create_db
echo "-----------------IMAS DB created------------"

. `pwd`/venv/bin/activate
# . $envs_dir/21_install_python_packages_for_tutorial.sh

echo "PIP_USER: $PIP_USER"
echo "PYTHONUSERBASE: $PYTHONUSERBASE"
pip config list

which python
python -m pip -V
python -m site

python -m pip install --no-user --ignore-installed ".[docs]"

cd docs/

echo "--------Building Jupyter-Book-------------"
jupyter-book build .
