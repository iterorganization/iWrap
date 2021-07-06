#!/bin/sh --login
clear
set -a
# Prepare tests
echo "....................|--------------Prepare For Tests--------------|...................."
# Verify whether script is running on CI server
# Checks if $ONBAMBOO exists in environment
if [ -z "${ONBAMBOO+x}" ]
then
  echo "--------------------!!!--------------NOTBAMBOO--------------!!!--------------------"
else
  echo "++++++++++++++++++++!!!--------------ONBAMBOO--------------!!!++++++++++++++++++++"
  # Source and run scripts with environment vars for CI server
  set -e

  envs_dir=`pwd`/envs/iter-bamboo
  chmod a+x $envs_dir/00_load_imas_env.sh
  . $envs_dir/00_load_imas_env.sh
  . $envs_dir/10_python_set_env.sh
  . $envs_dir/03_report_module_list.sh
  cd tests/
fi
# Run Pytests
echo "====================|--------------Run Tests--------------|===================="
# Runs python pytest and logs results to junit xml file
python -m pip list
python -m pytest
#python -m pytest --junitxml=test_results.xml -v


