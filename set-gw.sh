module purge
module load cineca

#module load imasenv/3.39.0/gcc
module load imasenv/3.39.0-5.0.0-test/gcc

if ( ! $?AL_VERSION ) then
	setenv AL_VERSION $UAL_VERSION
endif

setenv PATH "${PWD}/bin:${PATH}"
setenv PYTHONPATH ${PWD}:${PYTHONPATH}
setenv IWRAP_HOME=$PWD


# setting environment for running examples
module load xmllib/3.3.1/gcc
module load firefox/111.0.1
imasdb iter
imasdb tmp
setenv PYTHONPATH $ITMWORK/IWRAP_ACTORS:$PYTHONPATH

./docs/scripts/create_db
