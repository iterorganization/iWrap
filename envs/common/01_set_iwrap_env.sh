#!/bin/sh

export PATH=${IWRAP_HOME}/bin:${PATH}

# Ensure build tools are available, then install iWrap (deps come from EasyBuild modules)
pip install --quiet setuptools setuptools_scm
pip install -e ${IWRAP_HOME} --no-deps -q
export TESTS_DIR="${IWRAP_HOME}/tests"

