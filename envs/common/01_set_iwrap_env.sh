#!/bin/sh

export PATH=${IWRAP_HOME}/bin:${PATH}

if [[ ! -n $AL_VERSION ]]; then
    export AL_VERSION=$UAL_VERSION
fi

export PYTHONPATH=${IWRAP_HOME}:${PYTHONPATH}
export TESTS_DIR="${IWRAP_HOME}/tests"

