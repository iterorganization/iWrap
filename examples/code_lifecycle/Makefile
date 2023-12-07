.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a code_lifecycle --actor-type python --data-type legacy -f ./code_lifecycle.yaml

actor-gui:
	iwrap-gui -a code_lifecycle -f ./code_lifecycle.yaml

native:
	make -C ./native_code clean
	make -C ./native_code