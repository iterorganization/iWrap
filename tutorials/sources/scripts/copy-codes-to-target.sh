#!/bin/bash

build_subpath="_build/$1"
markdowns_directory="markdowns"
destination_directory=""
source_directory=""

for source_directory in $(find "$markdowns_directory/" -not -path "${markdowns_directory}/${build_subpath}/*" -type d -iname "codes")
do
  destination_directory=$(echo "$source_directory" | sed "s|${markdowns_directory}/|${markdowns_directory}/${build_subpath}/|")

  if [[ -d $destination_directory ]]; then
    rm -rf "$destination_directory"  # Remove the existing directory
    echo "Removed existing directory: $destination_directory"
  fi

  # Copy the directory whether it previously existed or not
  cp -r "$source_directory" "$destination_directory"
  echo "Copied contents of '$source_directory' to '$destination_directory'"
done
