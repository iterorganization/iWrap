export # export all variables to 'sub-Makefiles'

ACTOR_NAME=basic_cpp
ACTOR_TYPE=muscle3-cpp
CODE_LANGUAGE=cpp
WRAPPED_CODE_NAME=basic

clean wrapped actor actor-gui wf-run check-output test:
	@if [ -z "$(TESTS_DIR)" ]; then \
		echo "Error: TESTS_DIR is not set"; \
		exit 1; \
	fi
	@echo "TESTS_DIR: $(TESTS_DIR)"

	@if [ ! -f "$(TESTS_DIR)/Makefile" ]; then \
		echo "Error: $(TESTS_DIR)/Makefile not found"; \
		exit 1; \
	fi
	$(MAKE) -f $(TESTS_DIR)/Makefile $@

