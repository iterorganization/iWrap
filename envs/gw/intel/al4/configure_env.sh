# Set up modules environment
source /usr/share/Modules/init/sh

# Set up environment
module load imasenv/3.39.0/gcc

# setting environment for running examples
module load firefox/111.0.1
module load json-fortran/8.4.0/intel/2020
module load jsoncpp/1.9.5/gcc/7.3.0

export CXX="icpc"
export FC="ifort"
export MPICXX="mpiicpc"
export MPIFC="mpiifort"
