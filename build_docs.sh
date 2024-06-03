#!/bin/bash

copy_codes() {
build_subpath="${IWRAP_HOME}/docs/_build/jupyter_execute/tutorial/"
markdowns_directory="${IWRAP_HOME}/docs/tutorial"

find "$markdowns_directory/" -not -path "${build_subpath}/*" -type d -iname "codes" | while read source_directory; do
  destination_directory="${source_directory/$markdowns_directory\//$build_subpath}"

  if [[ -d $destination_directory ]]; then
    rm -rf "$destination_directory"
  fi
  mkdir -p "$destination_directory"

  cp -r "$source_directory/"* "$destination_directory/"
  echo "Copied contents of '$source_directory' to '$destination_directory'"
done
}

VENV_DIR="tutorial_venv"
build_docs() {

    if [[ -d ${IWRAP_HOME}/${VENV_DIR} ]]; then
      rm -rf ${IWRAP_HOME}/${VENV_DIR}
    fi

    echo -e "\nMaking venv named: $VENV_DIR"
    python3 -m venv ${IWRAP_HOME}/${VENV_DIR}

    echo -e "\nActivating virtual environment '${VENV_DIR}'..."
    source "${IWRAP_HOME}/${VENV_DIR}/bin/activate"

    pip install --upgrade pip
    pip install --force-reinstall -r ${IWRAP_HOME}/docs/requirements.txt

    # Always build the JupyterBook
    jupyter-book build ${IWRAP_HOME}/docs

    copy_codes
    cp -r ${IWRAP_HOME}/docs/images ${IWRAP_HOME}/docs/_build/jupyter_execute/tutorial/

    deactivate
}

build_docs


