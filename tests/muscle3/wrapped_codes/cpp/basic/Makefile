AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

CFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC -DAL_MAJOR=$(AL_MAJOR)

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	IMAS_LIB_NAME=al-cpp
else ifeq ($(AL_MAJOR), 4)
	IMAS_LIB_NAME=imas-cpp
endif

LIBS =   `pkg-config --libs $(IMAS_LIB_NAME)`
INCLUDES = `pkg-config --cflags $(IMAS_LIB_NAME)`

all: libbasic.a

# LIBRARY COMPILATION
lib%.a: %.o
	$(AR) -rcs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(CXX) $(CFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)


# CLEAN DIRECTORY
clean:
	$(RM) *.o *.a














