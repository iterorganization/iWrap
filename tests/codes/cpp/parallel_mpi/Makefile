INCLUDES = -I. `pkg-config al-cpp --cflags`
LIBS = `pkg-config al-cpp --libs`

CXXFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

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
