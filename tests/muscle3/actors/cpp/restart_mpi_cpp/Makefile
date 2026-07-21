export # export all variables to 'sub-Makefiles'

ACTOR_NAME=restart_mpi_cpp
ACTOR_TYPE=muscle3-cpp

CODE_LANGUAGE=cpp
WRAPPED_CODE_NAME=parallel_mpi

test: | clean wrapped actor wf-run wf-restart check-output

clean wrapped actor actor-gui wf-run wf-restart check-output:
	$(MAKE) -f $(TESTS_DIR)/Makefile $@