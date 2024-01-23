module load imasenv/3.39.0-5.0.0-test/gcc
module load iwrap
module load firefox/111.0.1
export PYTHONPATH=$ITMWORK/IWRAP_ACTORS:$PYTHONPATH

imasdb tutorial_db

export AL_VERSION_MAJOR=5
./sources/scripts/create_db
