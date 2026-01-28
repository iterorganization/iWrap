# Set up environment
source ci-build/st00-defs.sh

# Set up environment
echo "--------------Module load iWrap--------------"
if [ "$COMPILER_VENDOR" == "intel" ]; then  

  # INTEL
  try  module load iWrap/0.10.0-intel-2023b

else

  # INTEL
  try module load iWrap/0.10.0-GCCcore-13.2.0

fi

