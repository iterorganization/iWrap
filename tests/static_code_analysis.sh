#!/bin/sh --login
# Task: Setup Environment and Run Pylint code check. Publish report artifacts with python modules and imas env modules.
set -e

envs_dir=`pwd`/envs/iter-bamboo
venv_path=$(realpath $envs_dir/../../venv)

echo "Environment scripts path: $envs_dir"
echo "VENV path: $venv_path"
chmod a+x $envs_dir/00_load_imas_env.sh
source $envs_dir/00_load_imas_env.sh

source $venv_path/bin/activate

if [ -x $venv_path/bin/python ]; then
		echo "Virtualenv ready!"
else
	echo "Virtual env not active!"
	exit 2
fi

echo -e Python virtualenv active: `which ${venv_path}/bin/python`
source $envs_dir/03_report_module_list.sh

# Run pylint code check
echo "~~~~~====================PYLINT CODE CHECK====================~~~~~"
# to be used with pylint 2.17.4
#$venv_path/bin/python -m pylint --errors-only --output-format=pylint_junit.JUnitReporter --output=pylint.xml iwrap
$venv_path/bin/python -m pylint -E --output-format=pylint_junit.JUnitReporter iwrap > pylint.xml

