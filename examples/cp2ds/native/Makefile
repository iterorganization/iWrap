F90=gfortran
COPTS = -g -O0  -fPIC
LIBS =   `pkg-config --libs imas-gfortran`
INCLUDES = `pkg-config --cflags imas-gfortran`

all: libcp2ds.a

libcp2ds.a: coreprofiles2distsource.o
	@ar -rcs $@ $^

coreprofiles2distsource.o: coreprofiles2distsource.f90
	$(F90) $(COPTS) -c -o $@ $^ ${INCLUDES} $(LIBS)

clean:
	rm -f *.o *.a

