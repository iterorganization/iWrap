
.PHONY: native wrapper

all: wrapper


native:
	make -C ./native_code clean
	make -C ./native_code install


wrapper: native
	make -C physics_ii/wrapper clean
	make -C physics_ii/wrapper

