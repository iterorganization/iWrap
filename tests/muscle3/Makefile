.PHONY: wrapped actor actor-gui wf-run check-output test
export

export TEST_DIR:=$(TESTS_DIR)/actors/$(CODE_LANGUAGE)/$(ACTOR_NAME)
LAST_SNAPSHOT:=$(lastword $(sort $(wildcard $(TEST_DIR)/run_$(ACTOR_NAME)_*/snapshots/snapshot_*.ymmsl)))


ACTOR_DIR:=$(TEST_DIR)/m3_actor
export PYTHONPATH:=$(ACTOR_DIR):$(PYTHONPATH)

test: | clean wrapped actor wf-run check-output


check-output:
	@echo "================ CHECKING TEST OUTPUT ========================="
	@cmp expected.out test.out &> /dev/null || \
	(echo 'ERROR: expected and real workflow outcome differs!' && exit 1 )
	@echo 'SUCCESS: Test PASSED'


wf-run:
	@echo "================ TEST FIRST RUN ========================="

	@$(RM) -f test.out
	muscle_manager --start-all --log-level DEBUG\
				 $(TESTS_DIR)/workflow/test.ymmsl \
				 $(TESTS_DIR)/workflow/test_macro.ymmsl \
				 $(TESTS_DIR)/actors/$(CODE_LANGUAGE)/$(ACTOR_NAME)/test_micro.ymmsl

wf-restart:
	@echo "================ TEST RESTART ========================="
	@$(RM) -f test.out
	muscle_manager --start-all --log-level DEBUG\
				 $(TESTS_DIR)/workflow/test.ymmsl \
				 $(TESTS_DIR)/workflow/test_macro.ymmsl \
				 $(TESTS_DIR)/actors/$(CODE_LANGUAGE)/$(ACTOR_NAME)/test_micro.ymmsl  \
				 $(LAST_SNAPSHOT)

actor:
	@echo "================ ACTOR BUILD ========================="
	iwrap     --actor-type  $(ACTOR_TYPE) --actor-name $(ACTOR_NAME) --install-dir $(ACTOR_DIR) --file $(TEST_DIR)/$(ACTOR_NAME).yaml

actor-gui:
	iwrap-gui --actor-type $(ACTOR_TYPE) --actor-name $(ACTOR_NAME) --install-dir  $(ACTOR_DIR) --file $(TEST_DIR)/$(ACTOR_NAME).yaml

wrapped:
	@echo "================ WRAPPED CODE BUILD ========================="
	@$(MAKE) -C $(TESTS_DIR)/wrapped_codes/$(CODE_LANGUAGE)/$(WRAPPED_CODE_NAME) clean
	@$(MAKE) -C $(TESTS_DIR)/wrapped_codes/$(CODE_LANGUAGE)/$(WRAPPED_CODE_NAME)

clean:
	#@$(RM) -rf ./reports
	@$(RM) -rf $(ACTOR_DIR)
	@$(RM) -rf ./run_$(ACTOR_NAME)_*
