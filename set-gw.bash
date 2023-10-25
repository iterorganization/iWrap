module purge
module load cineca

module load imasenv/3.39.0/gcc

if [[ ! -n $AL_VERSION ]]; then
    export AL_VERSION=$UAL_VERSION
fi

export PATH="$PWD/bin:$PATH"
export PYTHONPATH=$PWD:$PYTHONPATH

# setting environment for running examples
module load xmllib/3.3.1/gcc
imasdb iter
imasdb tmp
export PYTHONPATH=$ITMWORK/IWRAP_ACTORS:$PYTHONPATH
