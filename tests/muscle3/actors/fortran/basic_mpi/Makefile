export # export all variables to 'sub-Makefiles'

ACTOR_NAME=basic_mpi
ACTOR_TYPE=muscle3-fortran
CODE_LANGUAGE=fortran
WRAPPED_CODE_NAME=parallel_mpi

clean wrapped actor actor-gui wf-run wf-restart check-output test:
	$(MAKE) -f $(TESTS_DIR)/Makefile $@
