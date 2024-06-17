module purge
module load cineca

module load imasenv/3.39.0/gcc

if [[ ! -n $AL_VERSION ]]; then
    export AL_VERSION=$UAL_VERSION
fi

export PATH="$PWD/bin:$PATH"
export PYTHONPATH=$PWD:$PYTHONPATH
export IWRAP_HOME=$PWD


# setting environment for running examples
module load xmllib/3.3.1/gcc
module load firefox/111.0.1
module load json-fortran/8.4.0/gcc/7.3.0
module load jsoncpp/1.9.5

imasdb iter
imasdb tmp
export PYTHONPATH=$ITMWORK/IWRAP_ACTORS:$PYTHONPATH

./docs/scripts/create_db