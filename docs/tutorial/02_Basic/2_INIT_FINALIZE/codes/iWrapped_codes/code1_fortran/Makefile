all: | clean build lib

build: 
	gfortran -g -fPIC code1.f90 `pkg-config --cflags al-fortran` -c -o code_fortran.o

lib: 
	ar -rcs libcode_fortran.a code_fortran.o

clean:
	rm -f *.exe *.a *.mod *.o *~


