#!/bin/sh --login

# MODULE AVAIL
touch modules.txt
module avail -t &> modules.txt
echo "=================================END============================" &>> modules.txt
