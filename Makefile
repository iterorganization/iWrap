# Check the version from the git tag
VERSION := $(shell git describe --abbrev=4 --dirty)-b

# Site configuration
IWRAP_NAME ?= iwrap
IWRAP_HOME ?= $(HOME)/$(IWRAP_NAME)

# Check for the existence of the Python interpreter and its absolute path
PYTHON_CMD ?= python
PY_CMD := $(if $(PYTHON_CMD),$(shell command -v $(PYTHON_CMD) 2>/dev/null))
PY_VER := $(if $(PY_CMD),$(shell $(PY_CMD) -c 'print(".".join(str(i) for i in __import__("sys").version_info[:2]))' 2>/dev/null))

# Installation paths
INSTALL_PREFIX ?= $(HOME)/IWRAP_INSTALL_DIR/$(VERSION)
INSTALL_PY ?= $(INSTALL_PREFIX)/lib/python$(PY_VER)
INSTALL_MOD ?= $(IWRAP_HOME)/etc/modulefiles
MODULEFILE ?= $(IWRAP_NAME)/$(VERSION)

all: iwrap_build
install: install_dir install_iwrap install_module
uninstall: uninstall_iwrap uninstall_module

.PHONY: $(MODULEFILE) iwrap_build iwrap_deps help clean docs

install_dir:
ifdef INSTALL_DIR
	$(eval INSTALL_PREFIX = $(INSTALL_DIR)/$(VERSION))
	$(eval INSTALL_PY = $(INSTALL_PREFIX)/lib/python$(PY_VER))
	@echo INSTALL_DIR = $(INSTALL_DIR)
	@echo INSTALL_PREFIX = $(INSTALL_PREFIX)
endif

iwrap_build: | iwrap_deps
	$(PY_CMD) setup.py bdist_wheel --dist-dir=./dist/$(VERSION)

install_iwrap: iwrap_build
	install -d $(dir $INSTALL_PREFIX)
	$(PY_CMD) -m pip install $(wildcard ./dist/$(VERSION)/*.whl) --compile --prefix $(INSTALL_PREFIX)

install_module: module/$(MODULEFILE)

module/$(MODULEFILE): iwrap/resources/module/iWrap.in
	install -d $(dir $(INSTALL_PREFIX)/$@)
	sed -e "s;__VERSION__;$(VERSION);" \
		-e "s;__PY_VER__;$(PY_VER);" \
		-e "s;__INSTALL_PREFIX__;$(INSTALL_PREFIX);" \
		-e "s;__INSTALL_PY__;$(INSTALL_PY);" \
		-e "s;__IWRAP_NAME__;$(IWRAP_NAME);" \
		$< > $(INSTALL_PREFIX)/$@

iwrap_deps:
	$(if $(wildcard $(PY_CMD)),,$(error No $(PYTHON_CMD) ($$PYTHON_CMD) executable found in path, did you load any python module?))

help:
	@echo py-$(PY_VER)
	@echo py-$(VERSION)

clean:
	$(PY_CMD) setup.py clean
	rm -rf $(INSTALL_PREFIX)
	find . -type d -name '__pycache__' | xargs rm -r

docs:
	make -C docs docs

code-check:
	pylint -E ./iwrap

