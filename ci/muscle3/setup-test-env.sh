#!/bin/bash
echo "executing $(basename "$0")"

print_help() {
    echo -e "Usage:"
    echo -e "\t $0 [--use-installed|-i] [--help|-h]"
    echo 
    echo -e "optional arguments:"
    echo -e "\t -i, --use-installed    use PIP installed or already loaded instance of plugins"
    echo -e "\t -h, --help             show this help message and exit"
}


use_installed=false

while [ $# -gt 0 ]; do
  case "$1" in
    --use-installed|-i)
       echo "WARNING: Using pre-loaded instance of plugins!"
       use_installed=true
      ;;
    --help|-h)
      print_help
      exit 0
      ;;
    *)
      printf "ERROR: Incorrect parameter: \"$1\"\n"
      print_help
      # exit 1
  esac
  shift
done

# setup system environment
source ci/muscle3/st01-system-env.sh "$@" || exit 1

# look if iWrap has been already loaded / configured
if `which iwrap &> /dev/null` ; then
    echo "WARNING: Using pre-loaded 'iWrap' from `which iwrap`!"

else
    echo "INFO: iWrap was not loaded! Loading 'iWrap' module!"
    source ci/muscle3/st02-iwrap-load.sh "$@" || exit 1
fi

# check if 'local' or installed version should be used
# if $use_installed ; then
#     echo "WARNING: Using pre-loaded instance of plugins!"
# else
#     echo "INFO: Setting up plugins from local directory!"
#     source ci/muscle3/st03-m3plugins-local.sh "$@" || exit 1
# fi


