FFLAGS = -g -O0  -fPIC
LIBS =   `pkg-config --libs imas-$(FC)`
INCLUDES = `pkg-config --cflags imas-$(FC)`

all: libcp2ds.a

libcp2ds.a: coreprofiles2distsource.o
	@ar -rcs $@ $^

coreprofiles2distsource.o: coreprofiles2distsource.f90
	$(FC) $(FFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	rm -f *.o *.a

