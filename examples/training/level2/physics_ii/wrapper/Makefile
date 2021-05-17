# Generated with FC2K version 4.13.0

# we need actor name
ACTOR=physics_ii

FF=ifort
FC=ifort
CC=gcc

WRAPPER_DIR=$(dir $(realpath $(firstword $(MAKEFILE_LIST))))

JAVA_HOME=/cineca/prod/opt/tools/jdk/1.8.0_111/binary
CFLAG=-g -D_REENTRANT -fPIC -I$(JAVA_HOME)/include   -I$(JAVA_HOME)/include/linux

FLAGS=-g -fPIC
OPENMP=NO
OPENMPFLAG=

MOD_DIR=./build/

# Compiler options for gfortran
ifeq ($(FC),gfortran)
   FLAGS=-g -fPIC -fno-second-underscore -ffree-line-length-none -J$(MOD_DIR)
endif
# Compiler options for ifort                                                                                                          
ifeq ($(FC),ifort)                                                                                                                    
    FLAGS=-g -fPIC -module $(MOD_DIR)
endif 

ifeq ($(OPENMP),YES)
     OPENMPFLAG=-qopenmp
endif

FFLAGS=$(FLAGS) $(OPENMPFLAG) 

MAIN_LIB_DEF=../native/lib/libphysics_ii.a

INCLUDES=-I./include $(shell pkg-config  --cflags imas-ifort)
LIBS=$(shell pkg-config  --libs imas-ifort) 


all: lib/libphysics_ii.so

bin/physics_ii.exe: src/standalone.f90 build/RWTools.o build/fortrantools.o build/FortranWrap.o 
	@echo "   * $^ -> $@"
	@mkdir -p ./bin
	$(FF) $(FFLAGS) -o $@ -Wl,--start-group $^ $(INCLUDES) -I$(MOD_DIR) $(MAIN_LIB_DEF)  $(shell pkg-config  --libs xmllib)  $(LIBS) -Wl,--end-group -Wl,-rpath,$(WRAPPER_DIR)/lib/


build/RWTools.o: src/RWTools.f90 build/FortranWrap.o
	@echo "  * $< -> $@"
	$(FF) $(FFLAGS) -c -w $^ -o $@  $(INCLUDES)
	
build/fortrantools.o: src/fortrantools.f90 
	@echo "  * $^ -> $@"
	@mkdir -p ./build
	$(FF) $(FFLAGS) -c -w $^ -o $@  $(INCLUDES)
	
lib/libphysics_ii.so: build/FortranWrap.o  build/fortrantools.o 
	@echo "   * $^ -> $@"
	@mkdir -p ./lib
	$(FF) $(FFLAGS) -shared -o $@ -Wl,--start-group $^ $(MAIN_LIB_DEF) $(CUSTOM_LIBS_DEF) $(shell pkg-config  --libs xmllib)  $(LIBS) -Wl,--end-group -Wl,-rpath,$(WRAPPER_DIR)/lib/

build/FortranWrap.o: src/FortranWrap.f90 build/fortrantools.o 
	@echo "   * $< -> $@"
	$(FF) $(FFLAGS) -c -w $^ -o $@  $(INCLUDES)
        



clean: 
	-rm -f *.o build/* lib/*.so bin/* src/ual_info.c
	
