.PHONY: native

native:
	gfortran -g -c -o wrapped_code.o wrapped_code.f90 `pkg-config --cflags imas-gfortran`
	@ar -rcs libwrapped_code.a wrapped_code.o

actor:
	iwrap --actor-type muscle3 --actor-name m3_actor -f ./code_description.yaml

macro:
	gfortran -g -o macro.exe macro.f90 `pkg-config --cflags --libs imas-gfortran ymmsl libmuscle_fortran`


run:
	EXAMPLE_DIR=`pwd` muscle_manager --start-all example.ymmsl

clean:
	rm -rf *.o *.exe *.mod *.a run_*

