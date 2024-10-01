# Set up modules environment
source /usr/share/Modules/init/sh

module load cineca

# Set up environment
module load imasenv/3.39.0/gcc


# setting environment for running examples
module load xmllib/3.3.1/gcc
module load firefox/111.0.1
module load json-fortran/8.4.0/gcc/7.3.0
module load jsoncpp/1.9.5/gcc/7.3.0

export CXX="g++"
export FC="gfortran"
export MPICXX="mpicxx"
export MPIFC="mpifort"
