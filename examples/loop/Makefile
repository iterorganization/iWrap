.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 loop_wf.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a loop -f ./loop.yaml

actor-gui: native
	iwrap-gui -a loop -f ./loop.yaml

native:
	make -C ./native_code clean
	make -C ./native_code