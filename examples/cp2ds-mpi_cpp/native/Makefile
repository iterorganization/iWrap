CXX=mpicxx
CXXFLAGS=-std=c++11 -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS =   `pkg-config --libs imas-cpp`
INCLUDES = `pkg-config --cflags imas-cpp`

all: libcp2ds_mpi_cpp.a

libcp2ds_mpi_cpp.a: cp2ds_mpi.o
	@ar -rvs $@ $^

cp2ds_mpi.o: cp2ds_mpi.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	rm -f *.o *.a
