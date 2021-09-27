#!/bin/sh --login
echo "Setting ITER"
module purge
module load IMAS
module load lxml/4.6.2-GCCcore-10.2.0


export PATH=${PWD}/bin:${PATH}
export PYTHONPATH=$PWD:$PYTHONPATH


# setting environment for running examples
module load  XMLlib

mkdir -p $HOME/public/imasdb/tmp/3/0
mkdir -p $HOME/public/imasdb/iter/3/0

export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH

