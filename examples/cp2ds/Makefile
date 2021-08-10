.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 cp2ds_workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a core2dist -f ./cp2ds.yaml

actor-gui: native
	iwrap-gui -a core2dist -f ./cp2ds.yaml

native:
	make -C ./native clean
	make -C ./native