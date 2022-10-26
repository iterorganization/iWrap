#!/bin/sh --login
# Task: Setup Environment and Run Pylint code check. Publish report artifacts with python modules and imas env modules.
set -e

envs_dir=`pwd`/envs/iter-bamboo
echo "Environement scripts path: $envs_dir"
chmod a+x $envs_dir/00_load_imas_env.sh
. $envs_dir/00_load_imas_env.sh
. $envs_dir/10_python_set_env.sh
. $envs_dir/../../venv/bin/activate
BACK_PID=$!
max_retry=20
retry=0
while [ ${retry} -lt ${max_retry} ]; do
	if [ kill -0 $BACK_PID 2>/dev/null ]; then
		echo "Virtualenv ready!"
		sleep 1
	else
		echo "Waiting for virtualenv to activate! Retry: $retry"
		venv_path=$(realpath $envs_dir/../../venv)
		echo "Path to virtualenv: $venv_path"
		ls $venv_path
		(( retry = retry + 1 ))
		sleep 1
	fi
done

echo -e Python virtualenv active: `which python` 
. $envs_dir/03_report_module_list.sh

# Run pylint code check
echo "~~~~~====================PYLINT CODE CHECK====================~~~~~"
python -m pylint -E --output-format=pylint_junit.JUnitReporter iwrap > pylint.xml
