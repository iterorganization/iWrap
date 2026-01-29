# Set up environment
source ci/muscle3/st00-defs.sh
echo "executing $(basename "$0")"
export IWRAP_HOME=$(realpath "$(dirname ${BASH_SOURCE})/../..")

export PATH=${IWRAP_HOME}/bin:${PATH}

export PYTHONPATH=${IWRAP_HOME}:${PYTHONPATH}


echo "IWRAP_HOME: $IWRAP_HOME"
echo "PATH: $PATH"
echo "PYTHONPATH: $PYTHONPATH"

echo "IWRAP setup completed successfully"

