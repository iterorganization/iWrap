# Check the version from the git tag
VERSION := $(shell git describe --abbrev=4 --dirty)-b

# Site configuration
IWRAP_NAME ?= iwrap
IWRAP_HOME ?= $(HOME)/$(IWRAP_NAME)
# Environment variable usually set by imas module, if not set: $HOME/imas/
IMAS_HOME ?= $(HOME)/imas

# Check for the existence of the Python interpreter and its absolute path
PYTHON_CMD ?= python
PY_CMD := $(if $(PYTHON_CMD),$(shell command -v $(PYTHON_CMD) 2>/dev/null))
PY_VER := $(if $(PY_CMD),$(shell $(PY_CMD) -c 'print(".".join(str(i) for i in __import__("sys").version_info[:2]))' 2>/dev/null))

# Installation paths
INSTALL_PREFIX ?= $(HOME)/IWRAP_INSTALL_DIR/$(VERSION)
INSTALL_PY ?= $(INSTALL_PREFIX)/lib/python$(PY_VER)
MODULEFILE ?= $(IWRAP_NAME)/$(VERSION)
INSTALL_MOD ?= $(IMAS_HOME)/etc/modulefiles

all: iwrap_build
install: install_dir install_iwrap install_module
uninstall: uninstall_module uninstall_iwrap

.PHONY: build/module/$(MODULEFILE) iwrap_build help clean docs

install_dir:
ifdef INSTALL_DIR
	$(eval INSTALL_PREFIX = $(INSTALL_DIR)/$(VERSION))
	$(eval INSTALL_PY = $(INSTALL_PREFIX)/lib/python$(PY_VER))
endif

iwrap_build: build_deps
	$(PY_CMD) setup.py bdist_wheel --dist-dir=./dist/$(VERSION)
	@$(MAKE) build_deps_clear --no-print-directory
	@echo -e "\n\tIWRAP_BUILD FINISHED\n"

install_iwrap: install_dir iwrap_build
	install -d $(dir $(INSTALL_PREFIX))
	$(PY_CMD) -m pip install $(wildcard ./dist/$(VERSION)/*.whl) --compile --prefix $(INSTALL_PREFIX)
	@echo -e "\n\tIWRAP_INSTALL FINISHED\n"
	@echo -e "\t iWrap installed in:\n\t$(INSTALL_PREFIX)\n"

install_module: build/module/$(MODULEFILE)
	install -d $(dir $(INSTALL_MOD)/$(MODULEFILE))
	install $< $(INSTALL_MOD)/$(MODULEFILE)
	@echo -e "\n\tINSTALL_MODULE FINISHED\n"
	@echo -e "\t iWrap ENVIRONMENT MODULE installed in:\n\t$(INSTALL_MOD)$(MODULEFILE)\n"

build/module/$(MODULEFILE): iwrap/resources/module/iWrap.in
	install -d $(dir $@)
	@sed -e "s;__VERSION__;$(VERSION);" \
		-e "s;__PY_VER__;$(PY_VER);" \
		-e "s;__INSTALL_PREFIX__;$(INSTALL_PREFIX);" \
		-e "s;__INSTALL_PY__;$(INSTALL_PY);" \
  		-e "s;__IWRAP_NAME__;$(IWRAP_NAME);" \
		$< > $@

build_deps:
	@$(PY_CMD) -m pip install -r requirements_build.txt --user

build_deps_clear:
	@$(PY_CMD) -m pip uninstall -r requirements_build.txt -y

uninstall_iwrap: install_dir
	rm -rf $(INSTALL_PREFIX)

uninstall_module: install_dir
	rm -rf $(INSTALL_PREFIX)/module/$(MODULEFILE)

help: install_dir
	@echo "USAGE: Run 'make all' to build iWrap. Run 'make install' to install."
	@echo -e "\tHINT: provide a INSTALL_DIR=[DIRECTORY...] with make command to specify installation directory."
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

clean: build_deps
	$(PY_CMD) setup.py clean
	@$(MAKE) build_deps_clear --no-print-directory
	find . -type d -name '__pycache__' | xargs rm -r

docs:
	make -C docs docs

code-check:
	pylint -E ./iwrap

