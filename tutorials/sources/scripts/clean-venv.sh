#!/bin/bash
if [[ -n "$VIRTUAL_ENV" ]]; then
    echo "We are in an active virtual environment located at $VIRTUAL_ENV"
    deactivate
else
    echo "We are not in an active virtual environment."
fi



echo "Cleaning tutorial environment"



# Delete the virtual environment directory
if [[ -d "tutorials_venv" ]] || [[ -L "tutorials_venv" ]]; then
    rm -rf "tutorials_venv/"
    if [[ ! -e "tutorials_venv" ]]; then
        echo "- 'tutorials_venv/' has been deleted successfully."
    else
        echo "- Failed to delete tutorials_venv/."
    fi
fi

# Delete the markdowns build directory
if [[ -d "markdowns/_build" ]] || [[ -L "markdowns/_build" ]]; then
    rm -rf "markdowns/_build/"
    if [[ ! -e "markdowns/_build" ]]; then
        echo "- 'markdowns/_build/' has been deleted successfully."
    else
        echo "- Failed to delete markdowns/_build/."
    fi
fi

# Delete the 'book' symbolic link
if [[ -L "book" ]] || [[ -e "book" ]]; then
    rm "book"
    if [[ ! -e "book" ]]; then
        echo "- 'book' (symbolic link) has been deleted successfully."
    else
        echo "- Failed to delete 'book' symbolic link."
    fi
fi

# Delete the 'notebooks' symbolic link
if [[ -L "notebooks" ]] || [[ -e "notebooks" ]]; then
    rm "notebooks"
    if [[ ! -e "notebooks" ]]; then
        echo "- 'notebooks' (symbolic link) has been deleted successfully."
    else
        echo "- Failed to delete 'notebooks' symbolic link."
    fi
fi
