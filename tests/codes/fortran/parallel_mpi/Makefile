IMAS_PKG_NAME = al-fortran

FLAGS = -g -fPIC

INCLUDES=-I. `pkg-config $(IMAS_PKG_NAME) --cflags`
LIBS=`pkg-config $(IMAS_PKG_NAME) --libs`

SRCS = parallel_mpi.f90
OBJS = $(patsubst %.f90,%.o,$(SRCS))

all: libParallelMpi.a

lib%.a: $(OBJS)
	@$(AR) -rcs $@ $^

%.o: %.f90
	$(MPIFC) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.exe *.a *.mod *.o *~
