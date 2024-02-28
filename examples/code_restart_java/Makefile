.PHONY: native actor actor-gui wf-run

wf-run:
	@rm -rf wf_output.txt
	python3 workflow.py
	python3 workflow.py # restart !
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a code_restart_java -f ./code_restart_java.yaml

actor-gui:
	iwrap-gui -a code_restart_java -f ./code_restart_java.yaml

native:
	make -C ./wrapped_code clean
	make -C ./wrapped_code
