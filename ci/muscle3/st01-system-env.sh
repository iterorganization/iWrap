# Set up environment
source ci-build/st00-defs.sh

# Set up environment 
source /usr/share/Modules/init/sh
module use /work/imas/etc/modules/all

# Set up environment
echo "--------------Module load IMAS--------------"
if [ "$COMPILER_VENDOR" == "intel" ]; then  # INTEL


  try  module load XMLlib/3.3.1-intel-compilers-2023.2.1

  try  module load MUSCLE3/0.7.1-intel-2023b
  try  module load IMAS/3.41.0-4.11.10-intel-2020b   # <=  No IMAS for intel 2023b


  export CXX="icpc"
  export FC="ifort"
  export MPICXX="mpiicpc"
  export MPIFC="mpiifort"

else

  # GFORTRAN
  try module load XMLlib/3.3.1-GCC-13.2.0
  try module load MUSCLE3/0.7.1-foss-2023b
  try module load IMAS/3.41.0-4.11.10-foss-2023b

  export CXX="g++"
  export FC="gfortran"
  export MPICXX="mpicxx"
  export MPIFC="mpifort"
fi

if [[ ! -n $AL_VERSION ]]; then
    export AL_VERSION=$UAL_VERSION
fi

export AL_MAJOR="${AL_VERSION%.*.*}"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
export TESTS_DIR=$( cd -- "${SCRIPT_DIR}/../tests"  &> /dev/null && pwd )

echo TESTS_DIR: $TESTS_DIR
