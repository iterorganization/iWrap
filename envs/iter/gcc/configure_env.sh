#!/bin/sh --login
# GCC_MODULES must be set by the user (e.g. via Bamboo plan variable).

if [ -z "${GCC_MODULES}" ]; then
    echo "ERROR: GCC_MODULES is not set."
    echo ""
    echo "  Set this variable (e.g. in your Bamboo plan or shell) with all required modules:"
    echo "    IMAS-Fortran  IMAS-Cpp  IMAS-Java  IMAS-Matlab"
    echo "    IMAS-Python  Tkinter  lxml  XMLlib  JPype  json-fortran  JsonCpp  f90nml"
    echo ""
    echo "  Example:"
    echo "    export GCC_MODULES=\"IMAS-Fortran/5.6.0-foss-2023b-DD-4.1.1 IMAS-Cpp/5.6.0-foss-2023b-DD-4.1.1 ...\""
    return 1 2>/dev/null || exit 1
fi

for m in ${GCC_MODULES}; do module load ${m}; done

export CXX="g++"
export FC="gfortran"
export MPICXX="mpicxx"
export MPIFC="mpifort"
