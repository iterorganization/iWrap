.PHONY: build-iwrap install clean docs
VERSION := $(shell git describe --abbrev=4 --dirty)-b
IWRAP_NAME ?= iwrap

PYTHON_CMD ?= python
PY_CMD := $(if $(PYTHON_CMD), $(shell command -v $(PYTHON_CMD) 2>/dev/null))
PY_VER := $(if $(PY_CMD), $(shell $(PY_CMD) -c 'print(".".join(str(i) for i in __import__("sys").version_info[:2]))' 2>/dev/null))

INSTALL_PREFIX ?= $(HOME)/IWRAP_INSTALL_DIR/$(VERSION)
INSTALL_PY ?= $(INSTALL_PREFIX)/lib/python$(PY_VER)

build-iwrap:
ifdef INSTALL_DIR
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

clean:
	find . -type d -name '__pycache__' | xargs rm -r

docs:
	make -C docs docs

code-check:
	pylint -E ./iwrap

