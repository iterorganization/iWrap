module load  IMAS/3.39.0-5.0.0-foss-2020b
module load  iWrap/0.8.0-GCCcore-10.2.0
module load  XMLlib/3.3.1-GCC-10.2.0
export PYTHONPATH=/home/ITER/$USER/IWRAP_ACTORS/:$PYTHONPATH


imasdb tutorial_db
export AL_VERSION_MAJOR=5

./sources/scripts/create_db
