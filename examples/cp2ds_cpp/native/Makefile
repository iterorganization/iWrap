CXX=g++
CXXFLAGS=-std=c++11 -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS =   `pkg-config --libs imas-cpp`
INCLUDES = `pkg-config --cflags imas-cpp`

all: libcp2ds_cpp.a

libcp2ds_cpp.a: coreprofiles2distsource.o
	@ar -rvs $@ $^

coreprofiles2distsource.o: coreprofiles2distsource.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	rm -f *.o *.a
