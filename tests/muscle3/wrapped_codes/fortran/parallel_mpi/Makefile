IMAS_LIB_NAME = al-fortran

FLAGS= -g -fPIC # FPIC AND PREPROCESSING OPTIONS

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME)`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME)`

all: libparallel_mpi.a

lib%.a: %.o
	@$(AR) -rcs $@ $^

%.o: %.f90
	$(MPIFC) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.o *.a *.mod *~

