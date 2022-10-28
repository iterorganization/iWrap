module purge
module load cineca

module load imasenv/3.37.0/gcc


setenv PATH "${PWD}/bin:${PATH}"
setenv PYTHONPATH ${PWD}:${PYTHONPATH}

# setting environment for running examples
module load xmllib/3.3.1/gcc
imasdb iter
imasdb tmp
setenv PYTHONPATH $ITMWORK/IWRAP_ACTORS:$PYTHONPATH
