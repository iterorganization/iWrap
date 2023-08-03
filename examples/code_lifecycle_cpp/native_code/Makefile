CXXFLAGS = -std=c++11 -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS =   `pkg-config --libs imas-cpp`
INCLUDES = `pkg-config --cflags imas-cpp`

all: libcode_lifecycle_cpp.a

# LIBRARY COMPILATION
libcode_lifecycle_cpp.a: code_lifecycle.o
	@ar -rcs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)


# CLEAN DIRECTORY
clean:
	rm -f *.o *.a














