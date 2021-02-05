module purge
module load cineca
module load itm-python/3.6
module load IMAS/3.29.0
module load xmllib/3.3.1/intel/17.0
module load pycharm

if (! $?PYTHONPATH) then  
    setenv PYTHONPATH ""
endif

setenv PYTHONPATH $PWD/physics_ii/:$PYTHONPATH
setenv PYTHONPATH $PWD/../../common/python:$PYTHONPATH
