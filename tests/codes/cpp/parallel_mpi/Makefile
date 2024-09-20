AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	INCLUDES=-I. `pkg-config al-cpp --cflags`
	LIBS=`pkg-config al-cpp --libs`
else ifeq ($(AL_MAJOR), 4)
  INCLUDES=-I. `pkg-config imas-cpp --cflags`
  LIBS=`pkg-config imas-cpp --libs`
endif

CXXFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC -DAL_MAJOR=$(AL_MAJOR)

all: libParallelMpi.a

# LIBRARY COMPILATION
libParallelMpi.a: parallel_mpi.o
	ar -rcs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(MPICXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS) $(DBG)

# CLEAN DIRECTORY
clean:
	rm -f *.o *.a
