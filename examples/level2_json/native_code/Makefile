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
LIBS+=`pkg-config json-fortran --libs`
INCLUDE+=`pkg-config json-fortran --cflags`

# LIST OF FORTRAN FILES
OBJS_ACTOR = physics_ii.o

# LIBRARY COMPILATION
libphysics_ii.a: ${OBJS_ACTOR}
	rm -f libphysics_ii.a
	@ar -cr $@ *.o

# TEST THE CONSISTENCY OF THE XML AND XSD FILES FOR INPUT PARAMETERS
validate:
	xmllint --noout input/input_wrapper.xsd input/input_wrapper.xml
	xmllint --noout --schema input/input_wrapper.xsd input/input_wrapper.xml
	xmllint --noout input/input_physics.xsd input/input_physics.xml
	xmllint --noout --schema input/input_physics.xsd input/input_physics.xml

# COMPILE THE IMAS ACTOR
install: libphysics_ii.a
	cp libphysics_ii.a ../physics_ii/native/lib

# RULES FOR ALL OBJECT FILES
%.o:%.f90
	$(FC) $(FFLAGS) -c  $< $(INCLUDE)

# CLEAN DIRECTORY
clean:
	rm -f *.exe *.a *.mod *.o physics_ii.xml




