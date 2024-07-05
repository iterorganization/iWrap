#!/bin/sh --login
set -e

envs_dir=`pwd`/envs/iter-bamboo
chmod a+x $envs_dir/00_load_imas_env.sh
. $envs_dir/00_load_imas_env.sh


echo "-----------Create Python Virtual ENV-------------"
# Remove Virtual env if already exists
rm -rf venv
python -m venv --system-site-packages venv

echo "-----------Activate Python Virtual ENV-------------"
. `pwd`/venv/bin/activate

echo "-----------------PIP pylint-junit------------"
python -m pip install pylint

echo "-----------------PIP pylint-junit------------"
python -m pip install pylint-junit

echo "--------------PIP install junit-xml-2--------"
python -m pip install junit-xml-2

echo "--------------Report Python and pytest version--------------"
echo "Python version: `python --version`"


export python_path=`which python`
echo -e Using python from env: $python_path

