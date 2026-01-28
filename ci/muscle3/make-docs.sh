#!/bin/bash


source ci/muscle3/setup-test-env.sh  $@ || exit 1

echo "-----------Create Python Virtual ENV-------------"
# Remove Virtual env if already exists
rm -rf venv
python -m venv --system-site-packages venv
#python -m venv venv

echo "-----------Activate Python Virtual ENV-------------"
source `pwd`/venv/bin/activate

echo "-----------------PIP sphinx-rtd-theme------------"
pip install sphinx-rtd-theme


echo "--------------Report Python version--------------"
echo "Python version: `python --version`"
echo "Using python from: `which python`"


echo "--------------Build documentation--------------"
# PYTHONPATH set to allow sphinx find 'Read The Docs' theme
PYTHONPATH=`pwd`/venv/lib//python3.11/site-packages/:$PYTHONPATH make docs
# make docs

echo "-----------Remove Python Virtual ENV-------------"
rm -rf venv