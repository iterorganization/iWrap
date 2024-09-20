module purge
module load cineca

source ./envs/set-env.sh gw gcc 5

imasdb iter
imasdb tmp
export PYTHONPATH=$ITMWORK/IWRAP_ACTORS:$PYTHONPATH

./docs/scripts/create_db
