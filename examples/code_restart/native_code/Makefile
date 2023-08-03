ifeq ($(FC), ifort)
# INTEL - LINKS TO THE IMAS LIBRARY AND INCLUDE DIRECTORY
  F90=ifort
  F90FLAGS= -g -fPIC -fpp # FPIC AND PREPROCESSING OPTIONS
  INCLUDE=-I. `pkg-config imas-ifort --cflags`
  LIBS=`pkg-config imas-ifort --libs        `
else
# GFORTRAN - LINKS TO THE IMAS LIBRARY AND INCLUDE DIRECTORY
  F90=gfortran
  F90FLAGS= -g -fPIC -cpp # FPIC AND PREPROCESSING OPTIONS
  INCLUDE=-I. `pkg-config imas-gfortran --cflags`
  LIBS=`pkg-config imas-gfortran --libs         `
endif

# XMLLIB LIBRARIES
LIBS+=`pkg-config xmllib --libs`
INCLUDE+=`pkg-config xmllib --cflags`

# LIST OF FORTRAN FILES
OBJS_ACTOR = code_restart.o

# LIBRARY COMPILATION
libcode_restart.a: ${OBJS_ACTOR}
	@ar -cr $@ *.o

# COMPILE THE IMAS ACTOR
install: libcode_restart.a
	cp libcode_restart.a ../code_restart/native/lib

# RULES FOR ALL OBJECT FILES
%.o:%.f90
	$(F90) ${F90FLAGS} -c  $< $(INCLUDE)

# CLEAN DIRECTORY
clean:
	rm -f *.exe *.a *.mod *.o *~




