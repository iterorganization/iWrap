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

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME) ymmsl libmuscle_fortran`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME) ymmsl libmuscle_fortran`

all: test_macro.exe

%.o: %.f90
	$(FC) $(FLAGS) -c -o $@ $^ ${INCLUDES}

%.exe: %.o
	$(FC) $(FLAGS) -o $@ $^ $(LIBS)

clean:
	@$(RM) -f *.o *.exe

