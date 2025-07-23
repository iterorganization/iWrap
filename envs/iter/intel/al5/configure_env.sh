#!/bin/sh --login
# Set up ITER modules environment

# Set up environment
module load IMAS-AL-Fortran/5.4.0-intel-2023b-DD-4.0.0
module load IMAS-AL-Java/5.4.0-intel-2023b-DD-4.0.0
module load IMAS-AL-Cpp/5.4.0-intel-2023b-DD-4.0.0
module load IMAS-AL-Matlab/5.4.0-intel-2023b-DD-4.0.0
module load IMAS-Python/2.0.1-intel-2023b
module load Tkinter/3.11.5-GCCcore-13.2.0

module load XMLlib/3.3.2-intel-compilers-2023.2.1
module load lxml/4.9.3-GCCcore-13.2.0
module load JPype/1.5.0-iimkl-2023b
module load JsonCpp/1.9.5-GCCcore-13.2.0
module load json-fortran/8.5.2-intel-compilers-2023.2.1
module load f90nml/1.4.4-GCCcore-13.2.0


export CXX="icpx"
export FC="ifort"
export MPICXX="mpiicpx"
export MPIFC="mpiifort"
