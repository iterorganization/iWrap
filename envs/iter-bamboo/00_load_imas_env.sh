#!/bin/sh --login
# Set up ITER modules environment

# Set up environment
echo "--------------Module load IMAS--------------"
if [ "$COMPILER_VENDOR" == "intel" ]; then  # INTEL


  module load IMAS/3.41.0-4.11.10-intel-2020b
  module load XMLlib/3.3.1-intel-2020b
  module load JPype1/1.4.1-intel-2020b-Java-11

  module load lxml/4.9.3-GCCcore-13.2.0
  export CXX="icpx"
  export FC="ifort"
  export MPICXX="mpiicpx"
  export MPIFC="mpiifort"
else
# GFORTRAN
  module load IMAS/3.41.0-4.11.10-foss-2023b
  module load lxml/4.9.3-GCCcore-13.2.0
  module load JPype/1.5.0-gfbf-2023b
  module load XMLlib/3.3.1-GCC-13.2.0

  module load json-fortran/8.5.2-GCC-13.2.0
  module load JsonCpp/1.9.5-GCCcore-13.2.0
  module load f90nml/1.4.4-GCCcore-13.2.0

  export CXX="g++"
  export FC="gfortran"
  export MPICXX="mpicxx"
  export MPIFC="mpifort"
fi

echo "----- 00_load_imas_env.sh ----"
module list
echo "----- 00_load_imas_env.sh ----"
echo WHICH PYTHON `which python`
