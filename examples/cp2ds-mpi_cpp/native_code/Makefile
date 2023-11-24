AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

CXXFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC -DAL_MAJOR=$(AL_MAJOR)

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	LIBS =   `pkg-config --libs al-cpp`
	INCLUDES = `pkg-config --cflags al-cpp`
else ifeq ($(AL_MAJOR), 4)
	LIBS =   `pkg-config --libs imas-cpp`
	INCLUDES = `pkg-config --cflags imas-cpp`
endif

all: libcp2ds_mpi_cpp.a

libcp2ds_mpi_cpp.a: cp2ds_mpi.o
	@ar -rvcs $@ $^

cp2ds_mpi.o: cp2ds_mpi.cpp
	$(MPICXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	rm -f *.o *.a
