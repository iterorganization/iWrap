F90=mpiifort
COPTS = -g -O0 -assume no2underscore -fPIC -shared-intel
LIBS =   `pkg-config --libs imas-ifort`
INCLUDES = `pkg-config --cflags imas-ifort`

all: libcp2ds-mpi.a

libcp2ds-mpi.a: cp2ds-mpi.o
	@ar -rs $@ $^

cp2ds-mpi.o: cp2ds-mpi.f90
	$(F90) $(COPTS) -c -o $@ $^ ${INCLUDES} $(LIBS)

clean:
	rm -f *.o *.a

