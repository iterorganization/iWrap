# Minimal makefile for Sphinx documentation
#
# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build
GENERATEDDIR  = documentation_files
PROJECT_PATH  = ../iwrap

YAML_FILE = project_informations.yaml.tmp
AUTHOR_KEY = "author"
PROJECT_KEY = "project"
RELEASE_KEY = "release"
VERSION_KEY = "version"

# Set project informations
AUTHOR = ""
PROJECT = "iWrap"
VERSION = $(shell git describe --all)
RELEASE = $(shell git describe --all --long --dirty)

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile all setconfig apidocs

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#clean setconfig apidocs html
%: Makefile
	mkdir -p _static
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

apidocs: Makefile
	sphinx-apidoc -o "$(GENERATEDDIR)" "$(PROJECT_PATH)" --separate

setconfig:
	touch ${YAML_FILE}
	echo ${AUTHOR_KEY}: ${AUTHOR} >> ${YAML_FILE}
	echo ${PROJECT_KEY}: ${PROJECT} >> ${YAML_FILE}
	echo ${VERSION_KEY}: ${VERSION} >> ${YAML_FILE}
	echo ${RELEASE_KEY}: ${RELEASE} >> ${YAML_FILE}

clean:
	rm -rf $(GENERATEDDIR) $(SOURCEDIR)/_build/ $(SOURCEDIR)/_static/ $(YAML_FILE)

docs:
	make clean
	make setconfig
	make apidocs
	make html