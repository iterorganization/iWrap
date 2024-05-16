#!/bin/bash

copy_codes() {
build_subpath="./docs/_build/jupyter_execute/tutorial/"
markdowns_directory="./docs/tutorial"

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

VENV_DIR="docs_book_venv"
build_docs() {

    echo -e "\nMaking venv named: $VENV_DIR"
    python3 -m venv $VENV_DIR

    echo -e "\nActivating virtual environment '${VENV_DIR}'..."
    source "${VENV_DIR}/bin/activate"

    pip install --upgrade pip
    pip install --force-reinstall -r ./docs/requirements.txt

    # Always build the JupyterBook
    jupyter-book build ./docs

    copy_codes
    cp -r ./docs/images ./docs/_build/jupyter_execute/tutorial/

    deactivate
    rm -rf $VENV_DIR
}

build_docs


