# Check the version from the git tag
VERSION := $(shell git describe --abbrev=4 --dirty)-b

# Site configuration
IWRAP_NAME ?= iwrap
IWRAP_HOME ?= $(HOME)/$(IWRAP_NAME)

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
install: install_iwrap install_module
uninstall: uninstall_iwrap uninstall_module
module: $(MODULEFILE)

.PHONY: $(MODULEFILE) iwrap_build iwrap_deps help clean docs

iwrap_build: | iwrap_deps
	$(PY_CMD) setup.py bdist_wheel

install_iwrap: iwrap_build
ifdef INSTALL_DIR
	$(INSTALL_PREFIX) = $(INSTALL_DIR)/$(VERSION)
	$(INSTALL_PY) = $(INSTALL_PREFIX)/lib/python$(PY_VER)
endif
	install -d $(INSTALL_PREFIX)
	install -d $(INSTALL_PY)
	$(PYCMD)

install_module: module/$(MODULEFILE)

module/$(MODULEFILE): iwrap/resources/module/iWrap.in
	install -d $(dir $@)
	sed -e "s:__VERSION__;$(VERSION);" \
		-e "s;__PY_VER__;$(PY_VER);" \
		-e "s;__INSTALL_PREFIX__;$(INSTALL_PREFIX);" \
		-e "s;__INSTALL_PY__;$(INSTALL_PY);" \
		-e "s;__IWRAP_NAME__;$(IWRAP_NAME);" \
		$< > $@

iwrap_deps:
	$(if $(wildcard $(PY_CMD)),,$(error No $(PYTHON_CMD) ($$PYTHON_CMD) executable found in path, did you load any python module?))

help:
	echo

clean:
	find . -type d -name '__pycache__' | xargs rm -r

docs:
	make -C docs docs

code-check:
	pylint -E ./iwrap

