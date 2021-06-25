module purge
module load IMAS
module load lxml


export PATH=${PWD}/bin:${PATH}
export PYTHONPATH=$PWD:$PYTHONPATH


# setting environment for running examples
module load  XMLlib

mkdir -p $HOME/public/imasdb/tmp/3/0

export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH

