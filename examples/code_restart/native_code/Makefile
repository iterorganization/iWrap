AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

FFLAGS= -g -fPIC # FPIC AND PREPROCESSING OPTIONS

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	INCLUDE=-I. `pkg-config al-fortran --cflags`
	LIBS=`pkg-config al-fortran --libs`
else ifeq ($(AL_MAJOR), 4)
  INCLUDE=-I. `pkg-config imas-$(FC) --cflags`
  LIBS=`pkg-config imas-$(FC) --libs`
endif

# LIBRARIES
LIBS+=``
INCLUDE+=``

# LIST OF FORTRAN FILES
OBJS_ACTOR = code_restart.o

# LIBRARY COMPILATION
libcode_restart.a: $(OBJS_ACTOR)
	@ar -cr $@ *.o

# COMPILE THE IMAS ACTOR
install: libcode_restart.a
	cp libcode_restart.a ../code_restart/native/lib

# RULES FOR ALL OBJECT FILES
%.o:%.f90
	$(FC) ${FFLAGS} -c  $< $(INCLUDE)

# CLEAN DIRECTORY
clean:
	rm -f *.exe *.a *.mod *.o *~




