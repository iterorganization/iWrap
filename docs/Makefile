# Minimal makefile for Sphinx documentation
#
# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SOURCEDIR     = .
BUILDDIR      = _build
GENERATEDDIR  = documentation_files
PROJECT_PATH  = ../

YAML_FILE = project_informations.yaml
AUTHOR_KEY = "author"
PROJECT_KEY = "project"
RELEASE_KEY = "release"
VERSION_KEY = "version"

# Set project informations
AUTHOR = ""
PROJECT = "iWrap"
RELEASE = $(shell git describe --always --dirty)
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
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

apidocs: Makefile
	sphinx-apidoc -o "$(GENERATEDDIR)" "$(PROJECT_PATH)" --separate

setconfig:
	sed -r "s/^(\s*${AUTHOR_KEY}\s*:\s*).*/\1${AUTHOR}/" -i "$(YAML_FILE)"
	sed -r "s/^(\s*${PROJECT_KEY}\s*:\s*).*/\1${PROJECT}/" -i "$(YAML_FILE)"
	sed -r "s/^(\s*${RELEASE_KEY}\s*:\s*).*/\1${RELEASE}/" -i "$(YAML_FILE)"
	sed -r "s/^(\s*${VERSION_KEY}\s*:\s*).*/\1${VERSION}/" -i "$(YAML_FILE)"

clean:
	rm -rf $(GENERATEDDIR) $(SOURCEDIR)/_build/ $(SOURCEDIR)/_static/

docs:
	make clean
	make setconfig
	make apidocs
	make html