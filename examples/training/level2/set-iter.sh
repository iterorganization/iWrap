export ids_path=""
export PYTHONPATH=""

module purge
module load IMAS/3.29.0-4.8.4
module load  XMLlib/3.3.1-intel-2018a

mkdir -p  $PWD/tmp
export ids_path="$PWD/tmp;$ids_path"

export PYTHONPATH=$PWD/physics_ii/:$PYTHONPATH
export PYTHONPATH=$PWD/../../common/python:$PYTHONPATH
