#!/bin/sh --login

# MODULES LIST
touch modules_list.txt
touch pip_list.txt
module list -t &> modules_list.txt
python3 -m pip list &> pip_list.txt
echo "=================================END============================" &>> modules_list.txt