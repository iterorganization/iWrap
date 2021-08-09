.PHONY: native actor actor-gui wf-run

wf-run:
	python3 workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a physics_ii -f ./physics_ii.yaml

actor-gui: native
	iwrap-gui -a physics_ii -f ./physics_ii.yaml

native:
	make -C ./native_code clean
	make -C ./native_code