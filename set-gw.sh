module purge
module load cineca

module load IMAS/3.32.1


setenv PATH "${PWD}/bin:${PATH}"
setenv PYTHONPATH ${PWD}:${PYTHONPATH}

# setting environment for running examples
module load xmllib/3.3.1/intel/17.0
imasdb iter
imasdb tmp
setenv PYTHONPATH $HOME/IWRAP_ACTORS:$PYTHONPATH