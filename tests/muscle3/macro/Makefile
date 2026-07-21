IMAS_LIB_NAME = al-fortran

FLAGS= -g -fPIC # FPIC AND PREPROCESSING OPTIONS

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME) ymmsl libmuscle_fortran`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME) ymmsl libmuscle_fortran`

all: test_macro.exe

%.o: %.f90
	$(FC) $(FLAGS) -c -o $@ $^ ${INCLUDES}

%.exe: %.o
	$(FC) $(FLAGS) -o $@ $^ $(LIBS)

clean:
	@$(RM) -f *.o *.exe

