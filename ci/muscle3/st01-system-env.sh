# Set up environment
source ci/muscle3/st00-defs.sh
echo "executing $(basename "$0")"
if test -f /etc/profile.d/modules.sh ;then
. /etc/profile.d/modules.sh
else
. /usr/share/Modules/init/sh
fi
module purge

echo "--------------Module load IMAS--------------"
if [ "$COMPILER_VENDOR" == "intel" ]; then

  # INTEL_MODULES must be set by the user (e.g. via Bamboo plan variable).
  if [ -z "${INTEL_MODULES}" ]; then
    echo "ERROR: INTEL_MODULES is not set."
    echo ""
    echo "  Set this variable in your Bamboo plan with all required modules:"
    echo "    IMAS-AL-Fortran  IMAS-AL-Cpp  IMAS-AL-Java  IMAS-AL-Matlab"
    echo "    IMAS-Python  MUSCLE3  XMLlib  PyYAML  lxml"
    exit 1
  fi

  for m in ${INTEL_MODULES}; do try module load ${m}; done

  export CXX="icpc"
  export FC="ifort"
  export MPICXX="mpiicpc"
  export MPIFC="mpiifort"

else

  # GCC_MODULES must be set by the user (e.g. via Bamboo plan variable).
  if [ -z "${GCC_MODULES}" ]; then
    echo "ERROR: GCC_MODULES is not set."
    echo ""
    echo "  Set this variable in your Bamboo plan with all required modules:"
    echo "    IMAS-AL-Fortran  IMAS-AL-Cpp  IMAS-AL-Java  IMAS-AL-Matlab"
    echo "    IMAS-Python  MUSCLE3  XMLlib  PyYAML  lxml"
    exit 1
  fi

  for m in ${GCC_MODULES}; do try module load ${m}; done

  export CXX="g++"
  export FC="gfortran"
  export MPICXX="mpicxx"
  export MPIFC="mpifort"
fi

export AL_MAJOR="${AL_VERSION%.*.*}"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

export TESTS_DIR=$( cd -- "${SCRIPT_DIR}/../../tests/muscle3" &> /dev/null && pwd )

echo TESTS_DIR: $TESTS_DIR
