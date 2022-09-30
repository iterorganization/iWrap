#!/bin/sh --login

echo "--------------PIP install Pylint--------------"
module load Pylint/2.7.4-GCCcore-10.2.0
echo "--------------Module load lxml--------------"
module load lxml/4.6.2-GCCcore-10.2.0

echo "--------------PIP install junit-xml-2--------"
python -m pip install junit-xml-2 

echo "--------------Report Python and pytest version--------------"
echo "Python version: `python --version`"
