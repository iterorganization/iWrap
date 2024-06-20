#!/bin/bash

# Function to display help message
display_help() {
    echo "Usage: $0 [OPTION]..."
    echo "Build the JupyterBook and start JupyterLab."
    echo
    echo "Options:"
    echo "  -h, --help    display this help and exit"
    echo "  --no-cache    delete _build/ (cache) directory before generating JupyterBook"
}

# Process command line argument
if [[ $1 == "-h" || $1 == "--help" ]]; then
    display_help
    exit 0
fi

# Define the virtual environment directory
VENV_DIR="tutorial_venv"
# Function to activate virtual environment and build the JupyterBook
activate_and_build() {
    echo -e "\nActivating virtual environment '${VENV_DIR}'..."
    source "${VENV_DIR}/bin/activate"

    # Check for --no-cache argument and delete _build directory if present
    if [[ $1 == "--no-cache" ]]; then
        echo "Deleting _build/ directory to remove cache..."
        rm -rf _build/
    fi

    # Always build the JupyterBook
    echo "Building JupyterBook..."
    jupyter-book build .
    echo -e "\nJupyterBook is successfully built!\n"

    echo -e "\nCopying 'codes' dir to proper folders in 'notebooks'\n"
    echo $(pwd)
    source scripts/copy-codes-to-JLab.sh "jupyter_execute"

    # Copy images to the JupyterBook build directory for notebooks
    echo -e "\nCopying images to the JupyterBook notebook directory..."
    cp -r images _build/jupyter_execute/tutorial/

    echo -e "\nOpening HTML and JupyterLab!\n"
    firefox _build/html/index.html &
    jupyter lab --ip=0.0.0.0 --notebook-dir=_build/jupyter_execute/tutorial/ --port=8888 --allow-root --IdentityProvider.token='' --ServerApp.allow_origin='*' --ServerApp.trust_xheaders='True' --browser=firefox --port-retries=100
}

# Check if virtual environment directory exists
if [[ ! -d $VENV_DIR ]]; then
    echo "Creating new Virtual Environment called '${VENV_DIR}'..."
    python3 -m venv $VENV_DIR

    # Activate virtual environment
    source "${VENV_DIR}/bin/activate"

    # Upgrade pip and install requirements
    echo "Upgrading pip and installing requirements..."
    pip install --upgrade pip
    pip install --force-reinstall -r requirements.txt
else
    echo "Virtual environment '${VENV_DIR}' exists."
fi

# Pass --no-cache argument to the activate_and_build function if present
if [[ $1 == "--no-cache" ]]; then
    activate_and_build "--no-cache"
else
    activate_and_build
fi