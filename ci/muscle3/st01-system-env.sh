# Set up environment
source ci/muscle3/st00-defs.sh
echo "executing $(basename "$0")"
# Set up environment such that module files can be loaded
if test -f /etc/profile.d/modules.sh ;then
. /etc/profile.d/modules.sh
else
. /usr/share/Modules/init/sh
fi
module purge

# Set up environment
echo "--------------Module load IMAS--------------"
if [ "$COMPILER_VENDOR" == "intel" ]; then  # INTEL

  try module load XMLlib/3.3.1-intel-compilers-2023.2.1

  try module load MUSCLE3/0.7.1-intel-2023b
  try module load IMAS-AL-Fortran/5.4.0-intel-2023b-DD-4.0.0
  try module load IMAS-AL-Java/5.4.0-intel-2023b-DD-4.0.0
  try module load IMAS-AL-Cpp/5.4.0-intel-2023b-DD-4.0.0
  try module load IMAS-AL-Matlab/5.4.0-intel-2023b-DD-4.0.0
  try module load IMAS-Python/2.0.1-intel-2023b
  try module load PyYAML/6.0.1-GCCcore-13.2.0

  export CXX="icpc"
  export FC="ifort"
  export MPICXX="mpiicpc"
  export MPIFC="mpiifort"

else

  # GFORTRAN
  try module load IMAS-AL-Fortran/5.4.0-foss-2023b-DD-4.0.0
  try module load IMAS-AL-Java/5.4.0-foss-2023b-DD-4.0.0
  try module load IMAS-AL-Cpp/5.4.0-foss-2023b-DD-4.0.0
  try module load IMAS-AL-Matlab/5.4.0-foss-2023b-DD-4.0.0
  try module load IMAS-Python/2.0.1-foss-2023b
  try module load XMLlib/3.3.1-GCC-13.2.0
  try module load MUSCLE3/0.7.1-foss-2023b
  try module load PyYAML/6.0.1-GCCcore-13.2.0

  export CXX="g++"
  export FC="gfortran"
  export MPICXX="mpicxx"
  export MPIFC="mpifort"
fi

export AL_MAJOR="${AL_VERSION%.*.*}"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export TESTS_DIR=$( cd -- "${SCRIPT_DIR}/../../tests/muscle3" &> /dev/null && pwd )

echo TESTS_DIR: $TESTS_DIR
