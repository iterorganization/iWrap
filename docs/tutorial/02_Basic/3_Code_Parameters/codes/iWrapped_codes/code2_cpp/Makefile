all: | clean build lib

build: 
	g++ -g -fPIC -pthread code2.cpp `pkg-config --cflags al-cpp libxml-2.0` -c -o code_cpp.o

lib: 
	ar -rcs libcode_cpp.a code_cpp.o

clean:
	rm -f *.exe *.a *.o *~
