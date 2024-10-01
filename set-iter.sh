#!/bin/sh --login
echo "Setting ITER"
module purge


source ./envs/set-env.sh iter gcc 5

mkdir -p $HOME/public/imasdb/tmp/3/0
mkdir -p $HOME/public/imasdb/iter/3/0

export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH

./docs/scripts/create_db

