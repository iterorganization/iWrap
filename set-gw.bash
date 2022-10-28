module purge
module load cineca

module load imasenv/3.37.0/gcc


export PATH="$PWD/bin:$PATH"
export PYTHONPATH=$PWD:$PYTHONPATH

# setting environment for running examples
module load xmllib/3.3.1/gcc
imasdb iter
imasdb tmp
export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH
