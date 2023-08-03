#!/bin/sh --login
echo "Setting ITER"
module purge

export PATH=${PWD}/bin:${PATH}
export PYTHONPATH=$PWD:$PYTHONPATH

# setting environment for running examples

source ./envs/iter-bamboo/00_load_imas_env.sh

# TEST IF LOADING SUCCEEDED
if ! echo $LOADEDMODULES | tr : '\n' | grep '^IMAS/' >/dev//null ;then
  echo "IMAS module was not loaded." >&2
  exit 1
fi

mkdir -p $HOME/public/imasdb/tmp/3/0
mkdir -p $HOME/public/imasdb/iter/3/0

export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH

