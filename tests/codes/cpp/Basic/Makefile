INCLUDES = -I. `pkg-config al-cpp --cflags`
LIBS = `pkg-config al-cpp --libs`

CXXFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS += `pkg-config --libs libxml-2.0 jsoncpp`
INCLUDES += `pkg-config --cflags libxml-2.0 jsoncpp`

all: libBasic.a

# LIBRARY COMPILATION
libBasic.a: xmlIMAS.o ParametersUtils.o Basic.o
	ar -rcs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS) $(DBG)

# CLEAN DIRECTORY
clean:
	rm -f *.o *.a
