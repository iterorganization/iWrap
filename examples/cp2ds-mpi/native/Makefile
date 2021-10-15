F90=mpifort
COPTS = -g -fPIC
LIBS =   `pkg-config --libs imas-gfortran`
INCLUDES = `pkg-config --cflags imas-gfortran`

all: libcp2ds-mpi.a

libcp2ds-mpi.a: cp2ds-mpi.o
	@ar -rcs $@ $^

cp2ds-mpi.o: cp2ds-mpi.f90
	$(F90) $(COPTS) -c -o $@ $^ ${INCLUDES} $(LIBS)

clean:
	rm -f *.o *.a

