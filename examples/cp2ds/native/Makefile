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
#FFLAGS = -g -O0  -fPIC

all: libcp2ds.a

libcp2ds.a: coreprofiles2distsource.o
	@ar -rcs $@ $^

coreprofiles2distsource.o: coreprofiles2distsource.f90
	$(FC) $(FFLAGS) -c -o $@ $^ $(INCLUDE) $(LIBS)

clean:
	rm -f *.o *.a

