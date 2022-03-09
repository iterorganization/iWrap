.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a UpperCase -f ./UpperCase.yaml

actor-gui: native
	iwrap-gui -a UpperCase -f ./UpperCase.yaml

native:
	make -C ./native_code clean
	make -C ./native_code