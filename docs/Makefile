# Minimal makefile for Sphinx documentation
#
# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build
GENERATEDDIR  = docstring_files
PROJECT_PATH  = ../iwrap

YAML_FILE = project_informations.yaml.tmp
AUTHOR_KEY = "author"
PROJECT_KEY = "project"
VERSION_KEY = "version"

# Set project informations
AUTHOR = "author"
PROJECT = "iWrap"
VERSION = $(shell git describe --always --dirty)

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

.PHONY: help Makefile all setconfig apidocs

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
#clean setconfig apidocs html
%: Makefile
	mkdir -p _static
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O) -Q

apidocs: Makefile
	sphinx-apidoc -q -o "$(GENERATEDDIR)" "$(PROJECT_PATH)" --separate --templatedir templates/

setconfig:
	touch ${YAML_FILE}
	echo ${AUTHOR_KEY}: ${AUTHOR} >> ${YAML_FILE}
	echo ${PROJECT_KEY}: ${PROJECT} >> ${YAML_FILE}
	echo ${VERSION_KEY}: ${VERSION} >> ${YAML_FILE}

clean:
	rm -rf $(GENERATEDDIR) $(SOURCEDIR)/_build/ $(SOURCEDIR)/_static/ $(YAML_FILE)

html_to_md:
	python3 $(SOURCEDIR)/html_to_rst.py

copy_attachments:
	cp -r $(SOURCEDIR)/iwrap_confluence_documentation/attachments $(SOURCEDIR)/_build/html/iwrap_confluence_documentation/.

docs:
	make clean
	make setconfig
	make apidocs
	make html
	make copy_attachments
