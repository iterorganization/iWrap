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

# XMLLIB LIBRARIES
LIBS+=`pkg-config xmllib --libs`
INCLUDE+=`pkg-config xmllib --cflags`

# LIST OF FORTRAN FILES
OBJS_ACTOR = basic_methods.o

# LIBRARY COMPILATION
libbasic_methods.a: ${OBJS_ACTOR}
	@ar -cr $@ *.o

# COMPILE THE IMAS ACTOR
install: libbasic_methods.a
	cp libbasic_methods.a ../basic_methods/native/lib

# RULES FOR ALL OBJECT FILES
%.o:%.f90
	$(FC) ${FFLAGS} -c  $< $(INCLUDE)

# CLEAN DIRECTORY
clean:
	rm -f *.exe *.a *.mod *.o *~




