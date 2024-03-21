all: | clean build0 executable0 run

build0:
	gfortran -g -pthread code0.f90 `pkg-config --cflags al-fortran` -c -o code0.o

executable0:
	gfortran -g code0.o `pkg-config --libs al-fortran` -o code0.exe

run:
	./code0.exe

clean:
	rm -f *.exe *.a *.o *~
	rm -f *.exe *.a *.o *~
