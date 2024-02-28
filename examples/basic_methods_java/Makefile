.PHONY: native actor actor-gui wf-run

wf-run: actor
	@rm -rf wf_output.txt
	python3 workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a basic_methods_java -f ./basic_methods_java.yaml

actor-gui:
	iwrap-gui -a basic_methods_java -f ./basic_methods_java.yaml

native:
	make -C ./wrapped_code clean
	make -C ./wrapped_code
