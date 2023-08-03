#!/bin/sh --login
# Set up ITER modules environment

# Set up environment
echo "--------------Module load IMAS--------------"
if [ "$COMPILER_VENDOR" == "intel" ]; then  # INTEL
  module load XMLlib/3.3.1-intel-2020b
  module load lxml/4.6.2-GCCcore-10.2.0
  module load IMAS/3.39.0-4.11.5-intel-2020b

  export CXX="icpc"
  export FC="ifort"
  export MPICXX="mpiicpc"
  export MPIFC="mpiifort"
else
# GFORTRAN
  module load XMLlib/3.3.1-GCC-10.2.0
  module load lxml/4.6.2-GCCcore-10.2.0
  module load IMAS/3.39.0-4.11.5-foss-2020b
  export CXX="g++"
  export FC="gfortran"
  export MPICXX="mpicxx"
  export MPIFC="mpifort"
fi

echo "----- 00_load_imas_env.sh ----"
module list
echo "----- 00_load_imas_env.sh ----"
echo WHICH PYTHON `which python`
ldd `which python`
echo LOADED PYTHON VERSION: `python -V`
echo "----- 00_load_imas_env.sh --END--"
