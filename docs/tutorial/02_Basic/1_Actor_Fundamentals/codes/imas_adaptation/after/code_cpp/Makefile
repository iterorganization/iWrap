all: | clean build executable run

build:
	g++ -std=c++11 -g -pthread imas_cpp_test.cpp `pkg-config --cflags al-cpp` -c -o imas_cpp_test.o

executable:
	g++ -std=c++11 -g imas_cpp_test.o `pkg-config --libs al-cpp` -o imas_cpp_test.exe

run:
	./imas_cpp_test.exe

clean:
	rm -f *.exe *.a *.o *~
	rm -f *.exe *.a *.o *~