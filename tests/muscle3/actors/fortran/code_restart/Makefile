export # export all variables to 'sub-Makefiles'

ACTOR_NAME=code_restart
ACTOR_TYPE=muscle3-fortran
CODE_LANGUAGE=fortran
WRAPPED_CODE_NAME=basic

test: | clean wrapped actor wf-run wf-restart check-output

clean wrapped actor actor-gui wf-run wf-restart check-output:
	$(MAKE) -f $(TESTS_DIR)/Makefile $@