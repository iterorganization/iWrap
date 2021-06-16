module purge
module load IMAS/3.31.0-4.8.7

export PATH=${PWD}/bin:${PATH}
export PYTHONPATH=$PWD:$PYTHONPATH


# setting environment for running examples
module load  XMLlib/3.3.1-intel-2018a

imasdb tmp

export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH

