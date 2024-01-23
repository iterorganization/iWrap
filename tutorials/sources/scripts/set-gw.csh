module load imasenv/3.39.0/gcc
module load iwrap
module load firefox/111.0.1

setenv PYTHONPATH $ITMWORK/IWRAP_ACTORS:$PYTHONPATH

imasdb tutorial_db

setenv AL_VERSION_MAJOR 5
./sources/scripts/create_db
