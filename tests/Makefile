# Define bash shell
SHELL := /bin/bash
# Flag a target as out of date to run it every time
.PHONY: all tests code_check
# Define default targets in order to pass simple 'make'
all: tests code_check
# Target launches bash script to perform the tests
tests:
	./run_tests.sh

code_check
    ./run_code_check.sh