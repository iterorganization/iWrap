#!/bin/bash

# Help function to display usage and options
show_help() {
cat << EOF
Usage: ${0##*/} [OPTIONS]
Convert Jupyter Notebook files in a directory to Markdown format.

Options:
  -s, --source   Source directory containing Jupyter Notebook files. Default is './notebooks'.
  -t, --target   Target directory to store converted Markdown files. Default is './new_markdowns'.
  -h, --help     Display this help and exit.

Examples:
  ${0##*/} -s ./notebooks -t ./new_markdowns
  ${0##*/} --source=./notebooks --target=./other_target
EOF
}

# Function to convert Jupyter Notebook (.ipynb) files to Markdown
convert_notebooks_to_md(){
    SOURCE_DIR=$1
    TARGET_DIR=$2

    # Create the target directory if it doesn't exist
    mkdir -p "${TARGET_DIR}"

    # Recreate the directory structure of SOURCE_DIR in TARGET_DIR
    for DIRECTORY in $(find "${SOURCE_DIR}" -type d)
    do
        mkdir -p "${TARGET_DIR}/${DIRECTORY#${SOURCE_DIR}/}"
    done

    # Convert each .ipynb file to Markdown
    for FILE in $(find "${SOURCE_DIR}" -type f -name "*.ipynb" ! -iname "*checkpoint*")
    do
        MD_OUTPUT_FILE="${TARGET_DIR}/${FILE#${SOURCE_DIR}/}"
        MD_OUTPUT_FILE="${MD_OUTPUT_FILE%.*}.md"
        jupytext --to md:myst --output "${MD_OUTPUT_FILE}" "${FILE}"
    done
}

# Set default SOURCE_DIR and TARGET_DIR
SOURCE_DIR="./notebooks"
TARGET_DIR="./new_markdowns"

# Parse command-line options for setting SOURCE_DIR and TARGET_DIR
while getopts ":s:t:h" opt; do
    case ${opt} in
        s) SOURCE_DIR=${OPTARG} ;;
        t) TARGET_DIR=${OPTARG} ;;
        h) show_help; exit 0 ;;
        \?) echo "Invalid option: -$OPTARG" >&2; exit 1 ;;
        :) echo "Option -$OPTARG requires an argument." >&2; exit 1 ;;
    esac
done

# Check if SOURCE_DIR exists; if not, exit with an error message
if [ ! -d "${SOURCE_DIR}" ]; then
    echo "Error: ${SOURCE_DIR}: directory doesn't exist" >&2
    exit 1
fi

# Start the conversion process
convert_notebooks_to_md "${SOURCE_DIR}" "${TARGET_DIR}"
