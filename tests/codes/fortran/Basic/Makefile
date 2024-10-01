AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

FLAGS = -g -fPIC

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	IMAS_PKG_NAME=al-fortran
else ifeq ($(AL_MAJOR), 4)
 	IMAS_PKG_NAME=imas-$(FC)
endif

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
