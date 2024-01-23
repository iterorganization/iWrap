#!/bin/bash

# Help function to display usage and options
show_help() {
cat << EOF
Usage: ${0##*/} [OPTIONS]
Convert Markdown files in a directory to HTML and Jupyter Notebook formats.

Options:
  -s, --source   Source directory containing Markdown files. Default is current directory.
  -t, --target   Target directory to store converted files. A 'converted' folder will be created here. Default is current directory.
  -h, --help     Display this help and exit.

Examples:
  ${0##*/} -s /path/to/source -t /path/to/target
  ${0##*/} --source=/path/to/source --target=/path/to/target
EOF
}

# Function to convert every Markdown (.md) file in a directory to HTML and Jupyter Notebook (.ipynb)
convert_struct(){
  
    # Recreate the directory structure of SOURCE_DIR in TARGET_DIR/converted
    for DIRECTORY in $(find "$1/" -type d)
    do
        mkdir -p "$2/iwrap_tutorial$DIRECTORY"
    done

    # Loop over each file in SOURCE_DIR
     for FILE in $(find "$1" -type f)
     do
        # Extract the file extension
         FILE_NAME=$(basename "$FILE")
         EXT="${FILE_NAME##*.}"
#         # If it's a Markdown file, convert it; otherwise, copy it to the new directory
         if [[ ${EXT} == "md" ]]; then
             echo $FILE
        #  #    #Convert extension of .md file to .ipynb with current path
            IPYNB_OUTPUT_FILE=$(echo ${FILE} | sed 's/\.md/\.ipynb/g')
             # sed -i -e '/^\[Next lesson\]/s/\.md/.ipynb/g' -e '/^\[Previous lesson\]/s/\.md/.ipynb/g' $FILE
            jupytext ${FILE} -q --to ipynb --output $2/iwrap_tutorial${IPYNB_OUTPUT_FILE}

         #    #convert extension of .md to .html with current path
            HTML_OUTPUT_FILE=$(echo ${FILE} | sed 's/\.md/\.html/g')
            # sed -i -e '/^\[Next lesson\]/s/\.ipynb/.html/g' -e '/^\[Previous lesson\]/s/\.ipynb/.html/g' $FILE
            mystnb-docutils-html -q --nb-read-as-md="yes" ${FILE} > $2/iwrap_tutorial${HTML_OUTPUT_FILE}

            #sed -i -e '/^\[Next lesson\]/s/\.html/.md/g' -e '/^\[Previous lesson\]/s/\.html/.md/g' $FILE

         
        else
              cp ${FILE} $2/iwrap_tutorial${FILE}
        fi
     done

    for FILE in $(find "$2/iwrap_tutorial" -type f \( -name "*.html" -o -name "*.ipynb" \) )
    do
         sed -i 's|http://localhost:8888/lab/tree/|&iwrap_tutorial/notebooks/|' $FILE
         FILE_NAME=$(basename "$FILE")
         EXT="${FILE_NAME##*.}"
        if [[ ${EXT} == "ipynb" ]]; then
            sed -i 's/http:\/\/localhost/http:\/\/localhost/g; s/\.md/\.ipynb/g' $FILE
        elif [[ ${EXT} == "html" ]]; then
            sed -i 's/http:\/\/localhost/http:\/\/localhost/g; s/\.md/\.html/g' $FILE
        fi
    done
}
# Declare SOURCE_DIR and TARGET_DIR variables, and initialize them to the current working directory
SOURCE_DIR=$(pwd)
TARGET_DIR=$(pwd)

# Your existing functions and main logic here...

# Parse command-line options for setting SOURCE_DIR and TARGET_DIR
while getopts s:t:h OPT; do
    case ${OPT} in
        s) SOURCE_DIR=${OPTARG};;  # Set SOURCE_DIR if -s option is provided
        t) TARGET_DIR=${OPTARG};;  # Set TARGET_DIR if -t option is provided
        h) show_help; exit 0;;     # Display help and exit if -h option is provided
    esac;
done


# Check if SOURCE_DIR exists; if not, exit with an error message
if [ ! -d ${SOURCE_DIR} ]; 
then
    echo "Error: ${SOURCE_DIR}: directory doesn't exist"
    exit 1
fi

# Check if TARGET_DIR exists; if not, create it
if [ ! -d ${TARGET_DIR} ]; 
then
    echo "Warning: ${TARGET_DIR}: directory doesn't exist"
    mkdir -p ${TARGET_DIR}
fi

convert_struct ${SOURCE_DIR} ${TARGET_DIR}
