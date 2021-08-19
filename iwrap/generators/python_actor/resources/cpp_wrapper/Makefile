# actor name
ACTOR_NAME={{actor_settings.actor_name}}

{% if code_description.language_specific.mpi %}
CXX=mpicxx
{% else %}
CXX=g++
{% endif %}


WRAPPER_DIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

CFLAG=-g -fpic
OPENMP=NO
OPENMPFLAG=

CXXFLAGS= -g -fpic -pthread $(OPENMPFLAG)


CODE_LIB_FILE=$(notdir {{actor_settings.code_description.code_path}})
CODE_LIB_DEF=../lib/$(CODE_LIB_FILE)


# main IMAS libs to be used
CORE_LIBS_FLAGS= $(shell pkg-config --cflags imas-cpp xmllib)
CORE_LIBS=$(shell pkg-config --libs imas-cpp xmllib)

# required 'platform' libs (i.e. published using pkg-config mechanism)
{% if actor_settings.code_description.language_specific.system_libraries %}
PLATFORM_LIBS_NAMES={{ (actor_settings.code_description.language_specific.system_libraries or []) | join(' ')}}
PLATFORM_LIBS_FLAGS=$(shell pkg-config --cflags $(PLATFORM_LIBS_NAMES))
PLATFORM_LIBS=$(shell pkg-config --libs $(PLATFORM_LIBS_NAMES))
{% else %}
PLATFORM_LIBS_NAMES=
PLATFORM_LIBS_FLAGS=
PLATFORM_LIBS=
{% endif %}



# required 'custom' libs (i.e. provided by user as a pth to library)
CUSTOM_LIBS={{ (actor_settings.code_description.language_specific.custom_libraries or [] ) | join(' ')}}


INCLUDES=-I./include $(CORE_LIBS_FLAGS) $(PLATFORM_LIBS)
LIBS=$(CORE_LIBS) $(PLATFORM_LIBS) $(CUSTOM_LIBS) $(PLATFORM_LIBS_FLAGS) $(CODE_LIB_DEF)


all:  lib/lib$(ACTOR_NAME).so  bin/$(ACTOR_NAME).exe

bin/$(ACTOR_NAME).exe: src/standalone.cpp build/serialization_tools.o build/cpp_wrapper.o build/iwrap_tools.o
	@echo "   * $^ -> $@"
	@mkdir -p ./bin
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $@ -Wl,--start-group $^  $(LIBS) -Wl,--end-group  #-Wl,-rpath,$(WRAPPER_DIR)/lib_ext/def/

lib/lib$(ACTOR_NAME).so: build/cpp_wrapper.o
	@echo "   * $^ -> $@"
	@mkdir -p ./lib
	$(CXX) $(CXXFLAGS) -shared -o $@  -Wl,--start-group $^  $(LIBS) -Wl,--end-group  #-Wl,-rpath,$(WRAPPER_DIR)/lib_ext/def/ -Wl,-rpath,$(WRAPPER_DIR)/lib_ext/opt/

build/cpp_wrapper.o: src/cpp_wrapper.cpp
	@echo "   * $^ -> $@"
	@mkdir -p build
	$(CXX) $(CXXFLAGS) -c -w $^ -o $@  $(INCLUDES)

build/iwrap_tools.o: src/iwrap_tools.cpp
	@echo "   * $^ -> $@"
	@mkdir -p build
	$(CXX) $(CXXFLAGS) -c -w $^ -o $@  $(INCLUDES)

build/serialization_tools.o: src/serialization_tools.cpp
	@echo "   * $^ -> $@"
	@mkdir -p build
	$(CXX) $(CXXFLAGS) -c -w $^ -o $@  $(INCLUDES)


clean: 
	-rm -f *.o build/* lib/*.so bin/*

