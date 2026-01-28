AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

FLAGS= -g -fPIC # FPIC AND PREPROCESSING OPTIONS

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	IMAS_LIB_NAME=al-fortran
else ifeq ($(AL_MAJOR), 4)
 	IMAS_LIB_NAME=imas-$(FC)
endif

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME)`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME)`

all: libbasic.a

lib%.a: %.o
	@$(AR) -rcs $@ $^

%.o: %.f90
	$(FC) $(FLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	@$(RM) -f *.exe *.a *.mod *.o *~




