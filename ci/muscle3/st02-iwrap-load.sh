# Set up environment
source ci/muscle3/st00-defs.sh

export IWRAP_HOME=$(realpath "$(dirname ${BASH_SOURCE})/../..")

export PATH=${IWRAP_HOME}/bin:${PATH}

export PYTHONPATH=${IWRAP_HOME}:${PYTHONPATH}
export TESTS_DIR="${IWRAP_HOME}/tests"


