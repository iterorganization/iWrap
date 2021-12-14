.PHONY: native actor actor-gui wf-run

wf-run:  workflow_cpp.py
	@rm -rf wf_output.txt
	python3 workflow_cpp.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a code_lifecycle_cpp -f ./code_lifecycle_cpp.yaml

actor-gui: native
	iwrap-gui -a code_lifecycle_cpp -f ./code_lifecycle_cpp.yaml

native:
	make -C ./native_code clean
	make -C ./native_code

clean:
	@rm -rf wf_output.txt
	make -C ./native_code clean
