IMAS_PKG_NAME = al-fortran

FLAGS = -g -fPIC

INCLUDES=-I. `pkg-config $(IMAS_PKG_NAME) --cflags`
LIBS=`pkg-config $(IMAS_PKG_NAME) --libs`

# XMLLIB LIBRARIES
LIBS += `pkg-config xmllib json-fortran --libs`
INCLUDES += `pkg-config xmllib json-fortran --cflags`

SRCS = ParametersUtils.f90 Basic.f90
OBJS = $(patsubst %.f90,%.o,$(SRCS))

all: libBasic.a

lib%.a: $(OBJS)
	@$(AR) -rcs $@ $^

%.o: %.f90
	$(FC) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.exe *.a *.mod *.o *~
