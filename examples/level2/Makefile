.PHONY: native actor actor-gui wf-run

wf-run:
	python3 workflow.py

actor:
	iwrap -a physics_ii -f ./physics_ii.yaml

actor-gui: native
	iwrap-gui -a physics_ii -f ./physics_ii.yaml

native:
	make -C ./native_code clean
	make -C ./native_code