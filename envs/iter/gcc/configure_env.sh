#!/bin/sh --login
# Set up ITER GCC modules environment
# GCC_MODULES must be set by the user (e.g. via Bamboo plan variable).
# Include ALL required modules — IMAS and dependencies — in this variable, e.g.:
#   GCC_MODULES="IMAS-AL-Fortran/5.6.0-foss-2023b-DD-4.1.1 IMAS-AL-Cpp/5.6.0-foss-2023b-DD-4.1.1 IMAS-AL-Java/5.6.0-foss-2023b-DD-4.1.1 IMAS-AL-Matlab/5.6.0-foss-2023b-DD-4.1.1 IMAS-Python/2.0.1-foss-2023b Tkinter/3.11.5-GCCcore-13.2.0 lxml/4.9.3-GCCcore-13.2.0 XMLlib/3.3.2-GCC-13.2.0 JPype/1.5.0-gfbf-2023b json-fortran/8.5.2-GCC-13.2.0 JsonCpp/1.9.5-GCCcore-13.2.0 f90nml/1.4.4-GCCcore-13.2.0"

if [ -z "${GCC_MODULES}" ]; then
    echo "ERROR: GCC_MODULES is not set."
    echo ""
    echo "  Set this variable (e.g. in your Bamboo plan or shell) with all required modules:"
    echo "    IMAS-AL-Fortran  IMAS-AL-Cpp  IMAS-AL-Java  IMAS-AL-Matlab"
    echo "    IMAS-Python  Tkinter  lxml  XMLlib  JPype  json-fortran  JsonCpp  f90nml"
    echo ""
    echo "  Example:"
    echo "    export GCC_MODULES=\"IMAS-AL-Fortran/5.6.0-foss-2023b-DD-4.1.1 IMAS-AL-Cpp/5.6.0-foss-2023b-DD-4.1.1 IMAS-AL-Java/5.6.0-foss-2023b-DD-4.1.1 IMAS-AL-Matlab/5.6.0-foss-2023b-DD-4.1.1 IMAS-Python/2.0.1-foss-2023b Tkinter/3.11.5-GCCcore-13.2.0 lxml/4.9.3-GCCcore-13.2.0 XMLlib/3.3.2-GCC-13.2.0 JPype/1.5.0-gfbf-2023b json-fortran/8.5.2-GCC-13.2.0 JsonCpp/1.9.5-GCCcore-13.2.0 f90nml/1.4.4-GCCcore-13.2.0\""
    return 1 2>/dev/null || exit 1
fi

for m in ${GCC_MODULES}; do module load ${m}; done

export CXX="g++"
export FC="gfortran"
export MPICXX="mpicxx"
export MPIFC="mpifort"
