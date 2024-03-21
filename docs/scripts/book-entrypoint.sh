#!/bin/bash

set -e

eval "$(/usr/bin/tclsh /usr/share/Modules/libexec/modulecmd.tcl bash load IMAS)"
source set-dock.sh

jupyter-book build .

## The -n or --no-dereference option treats LINK_NAME as a normal file if it is a symbolic link to a directory,
## and -f or --force removes existing destination files, which is safer for repeated script runs.
#ln -snf ./markdowns/_build/html/ book
#ln -snf ./markdowns/_build/jupyter_execute/ notebooks
#
#exec "$@"