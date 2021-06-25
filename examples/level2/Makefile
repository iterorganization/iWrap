.PHONY: native actor actor-gui

actor: native
	iwrap -a physics_ii -f ./physics_ii.yaml

actor-gui: native
	iwrap-gui -a physics_ii -f ./physics_ii.yaml

native:
	make -C ./native_code clean
	make -C ./native_code