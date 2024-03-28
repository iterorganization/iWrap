#!/bin/bash
set -e

eval "$(/usr/bin/tclsh /usr/share/Modules/libexec/modulecmd.tcl bash load IMAS)"

COPY_PATH="/opt/iwrap/docs"
cp -r  "${COPY_PATH}/documentation"           /docs/documentation
cp -r  "${COPY_PATH}/images/"                 /docs/images
cp -r  "${COPY_PATH}/scripts/"                /docs/
cp -r  "${COPY_PATH}//tutorial"               /docs/tutorial
cp -r  "${COPY_PATH}/_config.yml"             /docs/
cp -r  "${COPY_PATH}/_toc.yml"                /docs/
cp -r  "${COPY_PATH}/iWrap_introduction.md"   /docs/

source set-docker.sh

jupyter-book build .
echo -e  "\n JupyterBook is successfully  built!"

echo -e "\n Copying 'codes' dir to proper folders in 'notebooks'\n"
echo $(pwd)
source scripts/copy-codes-to-JLab.sh

# Copy images to the JupyterBook build directory for notebooks
echo -e "\nCopying images to the JupyterBook notebook directory..."
cp -r images _build/jupyter_execute/tutorial/

jupyter lab --ip=0.0.0.0 --notebook-dir=_build/jupyter_execute/tutorial/ --port=8888 --allow-root --IdentityProvider.token='' --ServerApp.allow_origin='*' --ServerApp.trust_xheaders='True' --browser=firefox --port-retries=100
