#!/bin/sh

# MODULES LIST
touch modules_list.txt
module list &> modules_list.txt
echo "=================================END============================" &>> modules_list.txt