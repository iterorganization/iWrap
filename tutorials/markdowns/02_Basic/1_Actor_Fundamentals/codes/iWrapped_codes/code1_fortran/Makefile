all: | clean build lib

build: 
	gfortran -g -fPIC code1.f90 `pkg-config --cflags imas-gfortran` -c -o code1.o

lib: 
	ar -rcs libcode_fortran.a code1.o

clean:
	rm -f *.exe *.a *.mod *.o *~


