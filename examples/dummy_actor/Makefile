.PHONY: native actor actor-gui wf-run

ACTOR=dummy_actor

wf-run:
	@rm -rf wf_output.txt
	python3 workflow.py
	@cmp expected_output.txt wf_output.txt &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )

actor:
	iwrap -a $(ACTOR) -f ./$(ACTOR).yaml

actor-gui: native
	iwrap-gui -a $(ACTOR) -f ./$(ACTOR).yaml

native:
