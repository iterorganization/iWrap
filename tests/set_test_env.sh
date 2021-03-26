#!/bin/sh
# Set up ITER modules environment
echo "--------------Set up ITER modules environment--------------"
set -e -a
# Set up environment
. /usr/share/Modules/init/sh
module use /work/imas/etc/modulefiles
module use /work/imas/etc/modules/all
module purge
echo "--------------Module load Python/3.6.4-intel-2018a--------------"
module load Python/3.6.4-intel-2018a
echo "--------------pip install pytest--------------"
python -m pip install pytest --user --ignore-installed
echo "--------------Python and pytest version--------------"
python --version
python -m pytest --version
echo "--------------------------------------------------------"