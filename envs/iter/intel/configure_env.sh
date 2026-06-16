#!/bin/sh --login
# INTEL_MODULES must be set by the user (e.g. via Bamboo plan variable).

if [ -z "${INTEL_MODULES}" ]; then
    echo "ERROR: INTEL_MODULES is not set."
    echo ""
    echo "  Set this variable (e.g. in your Bamboo plan or shell) with all required modules:"
    echo "    IMAS-Fortran  IMAS-Cpp  IMAS-Java  IMAS-Matlab"
    echo "    IMAS-Python  Tkinter  XMLlib  lxml  JPype  json-fortran  JsonCpp  f90nml"
    echo ""
    echo "  Example:"
    echo "    export INTEL_MODULES=\"IMAS-Fortran/5.5.0-intel-2023b-DD-4.1.0 IMAS-Cpp/5.5.0-intel-2023b-DD-4.1.0 ...\""
    return 1 2>/dev/null || exit 1
fi

for m in ${INTEL_MODULES}; do module load ${m}; done

export CXX="icpx"
export FC="ifort"
export MPICXX="mpiicpx"
export MPIFC="mpiifort"
