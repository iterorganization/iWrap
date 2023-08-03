ifeq ($(FC), ifort)
# INTEL - LINKS TO THE IMAS LIBRARY AND INCLUDE DIRECTORY
  FC=ifort
  FFLAGS= -g -fPIC -fpp # FPIC AND PREPROCESSING OPTIONS
  INCLUDE=-I. `pkg-config imas-ifort --cflags`
  LIBS=`pkg-config imas-ifort --libs        `
else
# GFORTRAN - LINKS TO THE IMAS LIBRARY AND INCLUDE DIRECTORY
  FC=gfortran
  FFLAGS= -g -fPIC -cpp # FPIC AND PREPROCESSING OPTIONS
  INCLUDE=-I. `pkg-config imas-gfortran --cflags`
  LIBS=`pkg-config imas-gfortran --libs         `
endif

# XMLLIB LIBRARIES
LIBS+=`pkg-config xmllib --libs`
INCLUDE+=`pkg-config xmllib --cflags`

# LIST OF FORTRAN FILES
OBJS_ACTOR = codeparam_input_physics.o loop.o

# LIBRARY COMPILATION
libloop.a: ${OBJS_ACTOR}
	rm -f libloop.a
	@ar -cr $@ *.o

# TEST THE CONSISTENCY OF THE XML AND XSD FILES FOR INPUT PARAMETERS
validate:
	xmllint --noout input/input_wrapper.xsd input/input_wrapper.xml
	xmllint --noout --schema input/input_wrapper.xsd input/input_wrapper.xml
	xmllint --noout input/input_physics.xsd input/input_physics.xml
	xmllint --noout --schema input/input_physics.xsd input/input_physics.xml

# COMPILE THE IMAS ACTOR
install: libloop.a
	cp libloop.a ../loop/native/lib

# RULES FOR ALL OBJECT FILES
%.o:%.f90
	$(FC) $(FFLAGS) -c  $< $(INCLUDE)

# CLEAN DIRECTORY
clean:
	rm -f *.exe *.a *.mod *.o loop.xml




