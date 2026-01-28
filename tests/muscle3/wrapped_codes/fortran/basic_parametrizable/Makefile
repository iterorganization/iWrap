FLAGS = -g -fPIC
LIBS =   `pkg-config --libs imas-$(FC)`
INCLUDES = `pkg-config --cflags imas-$(FC)`

all: libbasic_parametrizable.a

lib%.a: %.o
	@$(AR) -rcs $@ $^

%.o: %.f90
	$(FC) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.exe *.a *.mod *.o *~




