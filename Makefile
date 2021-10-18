.PHONY: build-iwrap install help clean docs

# Check the version from the git tag
VERSION := $(shell git describe --abbrev=4 --dirty)-b

# Site configuration
IWRAP_NAME ?= iwrap
IWRAP_HOME ?= $(HOME)/iwrap

# Check for the existence of the Python interpreter and its absolute path
PYTHON_CMD ?= python
PY_CMD := $(if $(PYTHON_CMD), $(shell command -v $(PYTHON_CMD) 2>/dev/null))
PY_VER := $(if $(PY_CMD), $(shell $(PY_CMD) -c 'print(".".join(str(i) for i in __import__("sys").version_info[:2]))' 2>/dev/null))

# Installation paths
INSTALL_PREFIX ?= $(HOME)/IWRAP_INSTALL_DIR/$(VERSION)
INSTALL_PY ?= $(INSTALL_PREFIX)/lib/python$(PY_VER)
INSTALL_MOD ?= $(IWRAP_HOME)/etc/modulefiles
MODULEFILE ?= $(IWRAP_NAME)/$(VERSION)

all: iwrap_build module

iwrap_build: | iwrap_deps
ifdef INSTALL_DIR
	$(PY_CMD) setup.py bdist_wheel
	@echo $(VERSION)
	@echo $(PYTHON_CMD)
	@echo $(PY_CMD)
	@echo $(PY_VER)
	@echo $(INSTALL_DIR)
else
	@echo "Please provide INSTALL_DIR=? argument!" >&2
endif

install:
ifdef INSTALL_DIR
	@echo $(INSTALL_DIR)
else
	@echo "USAGE: make $@ INSTALL_DIR=[PATH]"
	@echo -e " !ERROR: INSTALL_DIR=? unknown!" >&2
endif

module/$(MODULEFILE):
	echo %@

iwrap_deps:
	$(if $(wildcard $(PY_CMD)),,$(error No $(PYTHON_CMD) ($$PYTHON_CMD) executable found in path, did you load any python module?))

help:


clean:
	find . -type d -name '__pycache__' | xargs rm -r

docs:
	make -C docs docs

code-check:
	pylint -E ./iwrap

