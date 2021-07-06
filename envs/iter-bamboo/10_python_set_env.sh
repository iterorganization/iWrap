#!/bin/sh --login

#echo "--------------Module load GCC--------------"
#module load GCC
echo "--------------Module load Python--------------"
module load Python
echo "--------------PIP install Pylint--------------"
module load Pylint
echo "--------------Module load PyYAML--------------"
module load PyYAML
echo "--------------Module load lxml--------------"
module load lxml

echo "--------------Report Python and pytest version--------------"
echo "Python version: `python --version`"