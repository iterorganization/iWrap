AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

FFLAGS= -g -fPIC # FPIC AND PREPROCESSING OPTIONS

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	INCLUDE=-I. `pkg-config al-fortran --cflags`
	LIBS=`pkg-config al-fortran --libs`
else ifeq ($(AL_MAJOR), 4)
  INCLUDE=-I. `pkg-config imas-$(FC) --cflags`
  LIBS=`pkg-config imas-$(FC) --libs`
endif

all: libcp2ds-mpi.a

libcp2ds-mpi.a: cp2ds-mpi.o
	@ar -rcs $@ $^

cp2ds-mpi.o: cp2ds-mpi.f90
	$(MPIFC) $(FFLAGS) -c -o $@ $^ ${INCLUDE} $(LIBS)

clean:
	rm -f *.o *.a

