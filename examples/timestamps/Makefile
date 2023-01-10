.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 timestamps_workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a timestamps -f ./timestamps.yaml

actor-gui: native
	iwrap-gui -a timestamps -f ./timestamps.yaml

native:
	make -C ./native clean
	make -C ./native