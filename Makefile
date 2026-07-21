# Check the version from the git tag
VERSION ?= $(shell git describe --abbrev=4 --dirty)

# Site configuration
IWRAP_NAME ?= iwrap
IWRAP_HOME ?= $(HOME)/$(IWRAP_NAME)
# Environment variable usually set by imas module, if not set: $HOME/imas/
IMAS_HOME ?= $(HOME)/imas

# Check for the existence of the Python interpreter and its absolute path
PYTHON_CMD ?= python
PY_CMD := $(if $(PYTHON_CMD),$(shell command -v $(PYTHON_CMD) 2>/dev/null))
PY_VER := $(if $(PY_CMD),$(shell $(PY_CMD) -c 'print(".".join(str(i) for i in __import__("sys").version_info[:2]))' 2>/dev/null))
SHELL_SCRIPT := ./build_docs.sh
# Default installation paths
INSTALL_PREFIX ?= $(HOME)/IWRAP_INSTALL_DIR/$(VERSION)
INSTALL_PY ?= $(INSTALL_PREFIX)/lib/python$(PY_VER)
MODULEFILE ?= $(IWRAP_NAME)/$(VERSION)
INSTALL_MOD ?= $(HOME)/IWRAP_MODULE_DIR/

IWRAP_ALREADY_INSTALLED := $(shell which iwrap 2>/dev/null)

all: iwrap_build
install: install_dir install_iwrap install_module docs
uninstall: uninstall_module uninstall_iwrap

.PHONY: build/module/$(MODULEFILE) install_iwrap update_iwrap iwrap_build build_deps build_deps_clear help clean docs test-muscle3 build-muscle3-macro clean-muscle3


check_already_installed:
ifndef IWRAP_ALREADY_INSTALLED
	$(info "No existing iWrap installation found")
else
	$(error "iWrap is already installed. If you want to update iWrap, use 'make update_iwrap'")
endif

install_dir:
ifdef INSTALL_DIR
	$(eval INSTALL_PREFIX = $(INSTALL_DIR)/$(VERSION))
	$(eval INSTALL_PY = $(INSTALL_PREFIX)/lib/python$(PY_VER))
endif

iwrap_build:
	( \
		$(PY_CMD) -m venv .venv; \
		. .venv/bin/activate; \
		pip install build; \
		python -m build --wheel -o ./dist/$(VERSION); \
		deactivate; \
	)
	rm -rf .venv
	@echo -e "\n\tIWRAP_BUILD FINISHED\n"

install_iwrap: check_already_installed install_dir iwrap_build
	install -d $(dir $(INSTALL_PREFIX))
	$(PY_CMD) -m pip install $(wildcard ./dist/$(VERSION)/*.whl) --compile --prefix $(INSTALL_PREFIX)
	@echo -e "\n\tIWRAP_INSTALL FINISHED\n"
	@echo -e "\t iWrap installed in:\n\t$(INSTALL_PREFIX)\n"

update_iwrap: install_dir iwrap_build
	install -d $(dir $(INSTALL_PREFIX))
	$(PY_CMD) -m pip install $(wildcard ./dist/$(VERSION)/*.whl) --compile --force-reinstall --prefix $(INSTALL_PREFIX)
	@echo -e "\n\tIWRAP_UPDATE FINISHED\n"
	@echo -e "\t iWrap installed in:\n\t$(INSTALL_PREFIX)\n"

install_module: build/module/$(MODULEFILE)
	install -d $(dir $(INSTALL_MOD)/$(MODULEFILE))
	install $< $(INSTALL_MOD)/$(MODULEFILE)
	@echo -e "\n\tINSTALL_MODULE FINISHED\n"
	@echo -e "\t iWrap ENVIRONMENT MODULE installed in:\n\t$(INSTALL_MOD)/$(MODULEFILE)\n"

build/module/$(MODULEFILE): iwrap/resources/module/iWrap.in
	install -d $(dir $@)
	@sed -e "s;__VERSION__;$(VERSION);" \
		-e "s;__PY_VER__;$(PY_VER);" \
		-e "s;__INSTALL_PREFIX__;$(INSTALL_PREFIX);" \
		-e "s;__INSTALL_PY__;$(INSTALL_PY);" \
  		-e "s;__IWRAP_NAME__;$(IWRAP_NAME);" \
		$< > $@

uninstall_iwrap: install_dir
	rm -rf $(INSTALL_PREFIX)

uninstall_module:
	rm -rf $(INSTALL_MOD)/$(MODULEFILE)

help: install_dir
	@echo "USAGE: Run 'make all' to build iWrap. Run 'make install' to install."
	@echo -e "\tHINT: provide a INSTALL_DIR=[DIRECTORY...] with make command to specify installation directory."
	@echo -e "\tHINT: provide a INSTALL_MOD=[DIRECTORY...] with make command to specify a installation directory for iWrap module file."
	@echo "Do this make for each python build/installation."
	@echo -e '\n- Use the following flags to configure the build/install, which may be set'
	@echo "  previously on the command line for current environment:"
	@echo -e "\nOverride default python cmd, e. g. python3. \n\tPYTHON_CMD: [$(PYTHON_CMD)]"
	@echo -e "Installation dir (if given):"
	@echo -e "\tINSTALL_DIR: [$(if $(INSTALL_DIR),$(INSTALL_DIR),NOT GIVEN)]"
	@echo -e "Installation path prefix. This is different than installation directory which given is a parent directory."
	@echo -e "\tINSTALL_PREFIX: [$(INSTALL_PREFIX)]"
	@echo -e "Direct path to python module site-packages installation:"
	@echo -e "\tINSTALL_PY: [$(INSTALL_PY)]"
	@echo -e "Environment module file:"
	@echo -e "\tMODULEFILE: [$(MODULEFILE)]"
	@echo -e "Installation path for environment module file:"
	@echo -e "\tINSTALL_MOD: [$(INSTALL_MOD)]"
	@echo -e "Version of the package - iWrap version:"
	@echo -e "\tVERSION: [$(VERSION)]"
	@echo -e "\n- MUSCLE3-specific targets:"
	@echo -e "\ttest-muscle3        : Run MUSCLE3 integration tests"
	@echo -e "\tbuild-muscle3-macro : Build MUSCLE3 macro model for tests"
	@echo -e "\tclean-muscle3       : Clean MUSCLE3 test artifacts"

docs:
	@$(SHELL_SCRIPT)

code-check:
	pylint -E ./iwrap

# MUSCLE3-specific targets
test-muscle3:
	@echo "Running MUSCLE3 integration tests..."
	@if [ -f tests/run-muscle3-tests.sh ]; then \
		cd tests && ./run-muscle3-tests.sh; \
	else \
		echo "MUSCLE3 test script not found. Running pytest..."; \
		$(PY_CMD) -m pytest tests/muscle3/ -v -m muscle3 || echo "pytest not available or tests failed"; \
	fi

build-muscle3-macro:
	@echo "Building MUSCLE3 macro model for tests..."
	@if [ -d tests/muscle3/macro ]; then \
		$(MAKE) -C tests/muscle3/macro; \
	else \
		echo "MUSCLE3 macro directory not found at tests/muscle3/macro"; \
	fi

clean-muscle3:
	@echo "Cleaning MUSCLE3 test artifacts..."
	@$(RM) -rf tests/muscle3/run_*
	@$(RM) -rf tests/muscle3/*/m3_actor
	@$(RM) -rf tests/muscle3/actors/*/run_*
	@$(RM) -rf tests/muscle3/actors/*/*/m3_actor
	@$(RM) -rf tests/integration/muscle3/run_*
	@echo "MUSCLE3 test artifacts cleaned"

clean: clean-muscle3
	rm -rf dist
	rm -rf build
