.PHONY: native actor actor-gui wf-run

wf-run:  workflow_cpp.py
	@rm -rf wf_output.txt
	python3 workflow_cpp.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a physics_ii_cpp -f ./physics_ii_cpp.yaml

actor-gui: native
	iwrap-gui -a physics_ii_cpp -f ./physics_ii_cpp.yaml

native:
	make -C ./native_code clean
	make -C ./native_code