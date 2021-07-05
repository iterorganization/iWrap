#!/bin/sh --login

# MODULE AVAIL
touch modules.txt
module avail &> modules.txt
echo "=================================END============================" &>> modules.txt