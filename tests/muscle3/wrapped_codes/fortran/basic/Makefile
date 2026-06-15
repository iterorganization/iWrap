IMAS_LIB_NAME = al-fortran

FLAGS= -g -fPIC # FPIC AND PREPROCESSING OPTIONS

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME)`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME)`

all: libbasic.a

lib%.a: %.o
	@$(AR) -rcs $@ $^

%.o: %.f90
	$(FC) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.exe *.a *.mod *.o *~




