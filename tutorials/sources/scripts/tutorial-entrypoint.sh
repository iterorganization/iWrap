#!/bin/bash
set -e

eval "$(/usr/bin/tclsh /usr/share/Modules/libexec/modulecmd.tcl bash load IMAS)"

source set-dock.sh

jupyter-book build ./markdowns
echo -e  "\n JupyterBook is successfully  built!"

# The -n or --no-dereference option treats LINK_NAME as a normal file if it is a symbolic link to a directory,
# and -f or --force removes existing destination files, which is safer for repeated script runs.
ln -snf ./markdowns/_build/html/ book
ln -snf ./markdowns/_build/jupyter_execute/ notebooks
echo -e "\n Folders 'book' & 'notebooks' are successfully created"


echo -e "\n Copying 'codes' dir to proper folders in 'notebooks'"
source copy-codes-to-target.sh "jupyter_execute"

echo -e "\nOpening HTML version of tutorial in first Firefox tab.. \n"
firefox book/index.html &

echo -e "\nOpening JupyterLab version of tutorial in second Firefox tab.. \n"
jupyter lab --ip=0.0.0.0 --port=8888 --allow-root --IdentityProvider.token='' --ServerApp.allow_origin='*' --ServerApp.trust_xheaders='True'
