#!/bin/bash

# Define the base path for the build output, specifying that it should include 'tutorial/'
build_subpath="_build/jupyter_execute/tutorial/"
# Define the main content directory
markdowns_directory="tutorial"

# Find and loop over all 'codes' directories except those already within the build subpath.
find "$markdowns_directory/" -not -path "${build_subpath}/*" -type d -iname "codes" | while read source_directory; do
  # Replace the markdowns_directory with the full build_subpath to construct the destination directory.
  destination_directory="${source_directory/$markdowns_directory\//$build_subpath}"

  # Remove the existing destination directory if it exists.
  if [[ -d $destination_directory ]]; then
    rm -rf "$destination_directory"
  fi

  # Ensure the destination directory exists.
  mkdir -p "$destination_directory"

  # Copy the contents of the 'codes' directory to the destination.
  # The trailing slash in the source path ensures that the contents of the directory are copied rather than the directory itself.
  cp -r "$source_directory/"* "$destination_directory/"
  echo "Copied contents of '$source_directory' to '$destination_directory'"
done
