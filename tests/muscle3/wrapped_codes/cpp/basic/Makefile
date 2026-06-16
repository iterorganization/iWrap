IMAS_LIB_NAME = al-cpp

CFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME)`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME)`

all: libbasic.a

# LIBRARY COMPILATION
lib%.a: %.o
	$(AR) -rcs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(CXX) $(CFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)


# CLEAN DIRECTORY
clean:
	$(RM) *.o *.a
