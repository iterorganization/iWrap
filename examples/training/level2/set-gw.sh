setenv ids_path ""
setenv PYTHONPATH ""

module purge
module load cineca
module load itm-python/3.6
module load IMAS/3.31.0
module load xmllib/3.3.1/intel/17.0
module load pycharm

mkdir -p  $PWD/tmp
setenv ids_path "$PWD/tmp;$ids_path"

setenv PYTHONPATH $PWD/physics_ii/:$PYTHONPATH
setenv PYTHONPATH $PWD/../../common/python:$PYTHONPATH
