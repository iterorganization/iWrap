#!/bin/bash

function change_user_name() {
    find ./markdowns -type f -iname "*.md" -print0 | while IFS= read -r -d '' FILE
    do
        sed -i "s/$1/$2/g" "$FILE"
    done
}


if [[ ! -d "$PWD/tutorials_venv" ]]; then
    echo "Creating new Virtual Environment called 'tutorials_venv'"
    python3 -m venv tutorials_venv

    source tutorials_venv/bin/activate
    pip3 install --upgrade pip
    pip3 install --ignore-installed -r sources/scripts/requirements.txt


    python3 -m bash_kernel.install

    echo "Virtual environment "tutorials_venv" created"
    deactivate
else
    echo "Virtual env exists"
fi

change_user_name "iwrap_user" "$USER"

if [[ -d $PWD/tutorials_venv ]]; then

    echo -e "\nEntering existing virtual env 'tutorials_venv"
    source tutorials_venv/bin/activate

    jupyter-book build ./markdowns
    echo -e  "\n JupyterBook is successfully  built!\n"


    # The -n or --no-dereference option treats LINK_NAME as a normal file if it is a symbolic link to a directory,
    # and -f or --force removes existing destination files, which is safer for repeated script runs.
    ln -snf ./markdowns/_build/html/ book
    ln -snf ./markdowns/_build/jupyter_execute/ notebooks
    echo -e "\n Folders 'book' & 'notebooks' are successfully created"


    echo -e "\n Copying 'codes' dir to proper folders in 'notebooks'\n"
    source sources/scripts/copy-codes-to-target.sh "jupyter_execute"
fi

change_user_name "$USER" "iwrap_user"
