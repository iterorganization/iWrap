#!/bin/sh
# Set up ITER modules environment
set -e

# Set up environment
source /usr/share/Modules/init/sh

echo "--------------Module load IMAS--------------"
module load IMAS
echo "--------------Module load IDStools--------------"
module load IDStools