AL_MAJOR := $(firstword $(subst ., ,$(AL_VERSION)))

CXXFLAGS = -pthread -g -O0 -D__USE_XOPEN2K8 -fPIC -DAL_MAJOR=$(AL_MAJOR)

ifeq ($(AL_MAJOR),)
    $(error Could not extract major AL version from AL_VERSION system variable. Is your IMAS module loaded?)
else ifeq ($(AL_MAJOR), 5)
	LIBS =   `pkg-config --libs al-cpp`
	INCLUDES = `pkg-config --cflags al-cpp`
else ifeq ($(AL_MAJOR), 4)
	LIBS =   `pkg-config --libs imas-cpp`
	INCLUDES = `pkg-config --cflags imas-cpp`
endif


all: libcp2ds_cpp.a

libcp2ds_cpp.a: coreprofiles2distsource.o
	@ar -rcvs $@ $^

coreprofiles2distsource.o: coreprofiles2distsource.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^ $(INCLUDES) $(LIBS)

clean:
	rm -f *.o *.a
