CXX=g++
CXXFLAGS=-std=c++11 -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS =   `pkg-config --libs imas-cpp xmllib`
INCLUDES = `pkg-config --cflags imas-cpp xmllib`

all: libphysics_ii_cpp.a

# LIBRARY COMPILATION
libphysics_ii_cpp.a: codeparam_input_physics.o physics_ii.o
	@ar -rs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

# TEST THE CONSISTENCY OF THE XML AND XSD FILES FOR INPUT PARAMETERS
validate:
	xmllint --noout input/input_wrapper.xsd input/input_wrapper.xml
	xmllint --noout --schema input/input_wrapper.xsd input/input_wrapper.xml
	xmllint --noout input/input_physics.xsd input/input_physics.xml
	xmllint --noout --schema input/input_physics.xsd input/input_physics.xml

# CLEAN DIRECTORY
clean:
	rm -f *.o *.a














