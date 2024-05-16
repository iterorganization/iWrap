all: | clean build lib

build: 
	g++ -g -fPIC -pthread code2.cpp `pkg-config --cflags al-cpp` -c -o code2.o

lib: 
	ar -rcs libcode2.a code2.o

clean:
	rm -f *.exe *.a *.o *~