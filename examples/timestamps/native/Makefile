F90=gfortran
COPTS = -g -O0  -fPIC
LIBS =   `pkg-config --libs imas-gfortran`
INCLUDES = `pkg-config --cflags imas-gfortran`

all: libtimestamps.a

lib%.a: %.o
	@ar -rcs $@ $^

%.o: %.f90
	$(F90) $(COPTS) -c -o $@ $^ ${INCLUDES} $(LIBS)

clean:
	rm -f *.o *.a *.mod

