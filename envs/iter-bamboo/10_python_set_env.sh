#!/bin/sh --login

echo "--------------PIP install Pylint--------------"
module load Pylint
echo "--------------Module load lxml--------------"
module load lxml

echo "--------------Report Python and pytest version--------------"
echo "Python version: `python --version`"