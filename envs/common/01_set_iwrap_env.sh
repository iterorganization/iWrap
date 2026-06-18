#!/bin/sh

# Ensure we are inside a virtual env before pip installing
if [ -z "$VIRTUAL_ENV" ]; then
    python -m venv --system-site-packages ${IWRAP_HOME}/.venv
    . ${IWRAP_HOME}/.venv/bin/activate
fi
pip install --quiet setuptools setuptools_scm
pip install -e ${IWRAP_HOME} --no-deps -q
export TESTS_DIR="${IWRAP_HOME}/tests"

