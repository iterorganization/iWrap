all: | clean build0 executable0 run

build0:
	g++ -std=c++11 -g -pthread code0.cpp `pkg-config --cflags al-cpp` -c -o code0.o

executable0:
	g++ -std=c++11 -g code0.o `pkg-config --libs al-cpp` -o code0.exe

run:
	./code0.exe

clean:
	rm -f *.exe *.a *.o *~
	rm -f *.exe *.a *.o *~