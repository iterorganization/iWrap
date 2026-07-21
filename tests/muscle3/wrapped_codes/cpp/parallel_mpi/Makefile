IMAS_LIB_NAME = al-cpp

FLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME)`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME)`


all: libparallel_mpi.a

lib%.a: %.o
	@$(AR) -rvcs $@ $^

%.o: %.cpp
	$(MPICXX) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.o *.a
