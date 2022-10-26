#!/bin/sh --login
# Task: Setup Environment and Run Pylint code check. Publish report artifacts with python modules and imas env modules.
set -e

envs_dir=`pwd`/envs/iter-bamboo
venv_path=$(realpath $envs_dir/../../venv)
activate_venv () {
	source $venv_path/bin/activate
}

echo "Environement scripts path: $envs_dir"
chmod a+x $envs_dir/00_load_imas_env.sh
source $envs_dir/00_load_imas_env.sh
source $envs_dir/10_python_set_env.sh
activate_venv
max_retry=20
retry=0
venv_status=2
while [ ${retry} -lt ${max_retry} ]; do
	if [ -x $venv_path/bin/python ]; then
		echo "Virtualenv ready!"
		venv_status=0
		sleep 1
		break
	else
		echo "Waiting for virtualenv to activate! Retry: $retry"
		echo "Path to virtualenv: $venv_path"
		(( retry = retry + 1 ))
		sleep 1
	fi
done

if [ $venv_status -ne 0 ]; then
	echo "Virtual env not active!"
	exit $venv_status
fi

echo -e Python virtualenv active: `which python` 
source $envs_dir/03_report_module_list.sh

# Run pylint code check
echo "~~~~~====================PYLINT CODE CHECK====================~~~~~"
$venv_path/bin/python -m pylint -E --output-format=pylint_junit.JUnitReporter iwrap > pylint.xml
