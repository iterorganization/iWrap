F90=ifort
COPTS = -g -O0 -assume no2underscore -fPIC -shared-intel
LIBS =   `pkg-config --libs imas-ifort`
INCLUDES = `pkg-config --cflags imas-ifort`

all: libcp2ds.a

libcp2ds.a: coreprofiles2distsource.o
	@ar -rcs $@ $^

coreprofiles2distsource.o: coreprofiles2distsource.f90
	$(F90) $(COPTS) -c -o $@ $^ ${INCLUDES} $(LIBS)

clean:
	rm -f *.o *.a

