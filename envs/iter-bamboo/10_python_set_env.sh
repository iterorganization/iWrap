#!/bin/sh --login

echo "--------------Module load  Pylint--------------"
module load Pylint/2.7.4-GCCcore-10.2.0
echo "--------------Module load lxml--------------"
module load lxml/4.6.2-GCCcore-10.2.0

echo "--------------Report Python and pytest version--------------"
echo "Python version: `python --version`"
