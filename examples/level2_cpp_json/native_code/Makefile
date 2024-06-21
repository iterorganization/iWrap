AL_VERSION ?= $(UAL_VERSION)
AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

CXXFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC -DAL_MAJOR=$(AL_MAJOR)

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from UAL_VERSION/AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	LIBS =   `pkg-config --libs al-cpp`
	INCLUDES = `pkg-config --cflags al-cpp`
else ifeq ($(AL_MAJOR), 4)
	LIBS =   `pkg-config --libs imas-cpp`
	INCLUDES = `pkg-config --cflags imas-cpp`
endif

# LIBRARIES
LIBS+=`pkg-config --libs jsoncpp`
INCLUDE+=`pkg-config --cflags jsoncpp`

DBG ?= -DNDEBUG

all: liblevel2_cpp_json.a

# LIBRARY COMPILATION
liblevel2_cpp_json.a:  level2_cpp_json.o
	ar -rcs $@ $^

# RULES FOR ALL OBJECT FILES
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS) $(DBG)


# CLEAN DIRECTORY
clean:
	rm -f *.o *.a

