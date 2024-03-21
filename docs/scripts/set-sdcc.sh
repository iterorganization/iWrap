
module unload IMAS/3.39.0-4.11.7-foss-2020b
module load IMAS/3.39.0-5.0.0-foss-2020b
module load  iWrap/0.9.1-GCCcore-10.2.0
module load  XMLlib/3.3.1-GCC-10.2.0
export PYTHONPATH=/home/ITER/$USER/IWRAP_ACTORS/:$PYTHONPATH

export AL_VERSION_MAJOR=5

./scripts/create_db
