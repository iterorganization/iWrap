all: | clean build lib

build: 
	gfortran -g -fPIC code1.f90 `pkg-config --cflags al-fortran` -c -o code1.o

lib: 
	ar -rcs libcode1.a code1.o

clean:
	rm -f *.exe *.a *.mod *.o *~


