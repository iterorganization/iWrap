COMPILER ?= g++

.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 timestamps_cpp_workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a timestamps_cpp -f ./timestamps_$(COMPILER).yaml

actor-gui: native
	iwrap-gui -a timestamps_cpp -f ./timestamps_$(COMPILER).yaml

native:
	make -C ./native_code clean
	make -C ./native_code

clean:
	@rm -rf wf_output.txt
	make -C ./native_code clean
