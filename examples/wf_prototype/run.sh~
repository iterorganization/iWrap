#!/bin/bash

for i in distance/distance_fortran distance/distance_c_wrapper velocity/velocity_fortran velocity/velocity_c_wrapper; do
  pushd $i
  make clean
  make
  ./bin/main
  popd
done
