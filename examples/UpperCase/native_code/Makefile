ifeq ($(FC), ifort)
# INTEL - LINKS TO THE IMAS LIBRARY AND INCLUDE DIRECTORY
  FC=ifort
  FFLAGS= -g -fPIC -fpp # FPIC AND PREPROCESSING OPTIONS
  INCLUDE=-I. `pkg-config imas-ifort --cflags`
  LIBS=`pkg-config imas-ifort --libs        `
else
# GFORTRAN - LINKS TO THE IMAS LIBRARY AND INCLUDE DIRECTORY
  FC=gfortran
  FFLAGS= -g -fPIC -cpp  # FPIC AND PREPROCESSING OPTIONS
  INCLUDE=-I. `pkg-config imas-gfortran --cflags`
  LIBS=`pkg-config imas-gfortran --libs         `
endif

# XMLLIB LIBRARIES
LIBS+=`pkg-config xmllib --libs`
INCLUDE+=`pkg-config xmllib --cflags`

# LIST OF FORTRAN FILES
OBJS_ACTOR = UpperCase.o

# LIBRARY COMPILATION
libUpperCase.a: ${OBJS_ACTOR}
	@ar -cr $@ *.o

# COMPILE THE IMAS ACTOR
install: libUpperCase.a
	cp libUpperCase.a ../UpperCase/native/lib

# RULES FOR ALL OBJECT FILES
%.o:%.f90
	$(FC) $(FFLAGS) -c  $< $(INCLUDE)

# CLEAN DIRECTORY
clean:
	rm -f *.exe *.a *.mod *.o *~




