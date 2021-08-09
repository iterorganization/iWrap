.PHONY: native actor actor-gui wf-run

wf-run:  workflow_cpp.py
	python3 workflow_cpp.py

actor:
	iwrap -a physics_ii_cpp -f ./physics_ii_cpp.yaml

actor-gui: native
	iwrap-gui -a physics_ii_cpp -f ./physics_ii_cpp.yaml

native:
	make -C ./native_code clean
	make -C ./native_code