.PHONY: native actor actor-gui wf-run

ACTOR=basic_methods_cpp

wf-run:  workflow_cpp.py
	@rm -rf wf_output.txt
	python3 workflow_cpp.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a $(ACTOR) -f ./$(ACTOR).yaml

actor-gui:
	iwrap-gui -a $(ACTOR) -f ./$(ACTOR).yaml

native:
	make -C ./native_code clean
	make -C ./native_code

clean:
	@rm -rf wf_output.txt
	make -C ./native_code clean
