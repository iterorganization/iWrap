#!/bin/sh --login

echo "-----------------PIP pylint-junit------------"
python -m pip install pylint-junit 
echo "--------------PIP install junit-xml-2--------"
python -m pip install junit-xml-2 

echo "--------------Report Python and pytest version--------------"
echo "Python version: `python --version`"
