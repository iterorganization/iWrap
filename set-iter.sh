#!/bin/sh --login
echo "Setting ITER"
module purge



export PATH=${PWD}/bin:${PATH}
export PYTHONPATH=$PWD:$PYTHONPATH


# setting environment for running examples

module load IMAS 

# TEST IF LOADING SUCCEEDED
if ! echo $LOADEDMODULES | tr : '\n' | grep '^IMAS/' >/dev//null ;then
  echo "IMAS module was not loaded." >&2
  exit 1
fi

# CHOOSE THE COMPILER
if [ -z "$FCOMPILER" ]; then
    echo 'FCOMPILER not set'
    echo '=> Use GCC as default'
    export FCOMPILER=gfortran
else
   if [ "$FCOMPILER" == "ifort" ]; then
      echo '$FCOMPILER set to intel'
    else
      echo '$FCOMPILER set to gfortran'
    fi
fi

# INTEL
if [ "$FCOMPILER" == "ifort" ]; then
  module load XMLlib/3.3.1-intel-2018a
  OBJ=obj_ifort
else
# GFORTRAN
  # This mangling of modules is only necessary until the IMAS module is split into separate modules for each compiler
  # Unload intel dependencies
  module unload Python PyYAML Tkinter matplotlib iimpi
  # Load foss dependencies
  
  module load Python/3.8.6-GCCcore-10.2.0
  module load XMLlib/3.3.1-GCC-10.2.0
  module load gompi/2020b
  module load lxml/4.6.2-GCCcore-10.2.0

  OBJ=obj_gfortran
fi




mkdir -p $HOME/public/imasdb/tmp/3/0
mkdir -p $HOME/public/imasdb/iter/3/0

export PYTHONPATH=$HOME/IWRAP_ACTORS:$PYTHONPATH

