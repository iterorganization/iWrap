FFLAGS = -g -fPIC
LIBS =   `pkg-config --libs imas-$(FC)`
INCLUDES = `pkg-config --cflags imas-$(FC)`

all: libcp2ds-mpi.a

libcp2ds-mpi.a: cp2ds-mpi.o
	@ar -rcs $@ $^

cp2ds-mpi.o: cp2ds-mpi.f90
	$(MPIFC) $(FFLAGS) -c -o $@ $^ ${INCLUDES} $(LIBS)

clean:
	rm -f *.o *.a

