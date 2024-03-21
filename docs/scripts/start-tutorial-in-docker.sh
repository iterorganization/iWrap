#!/bin/bash

# Function to display help information
function display_help {
  echo "Usage: $0 [options]"
  echo "Options:"
  echo "  -i <IMAGE_NAME>        Set the Docker image name; default is 'iwrap_tutorial'"
  echo "  -v <IMAGE_VERSION>     Set which Docker image version will be used; default is 'AL5'"
  echo "  -c <CONTAINER_NAME>    Set the container name; if not specified, it will be the same as image name"
  echo "  -p <HOST_PORT>         Set the host port to map to container's 8888; if not specified, defaults to 8888"
  echo "  -h                     Display this help message and exit"
  exit 1
}

# Set default image name and port
IMAGE_NAME="iwrap_tutorial"
IMAGE_VERSION="AL5"
IMAGE_TAG=""
CONTAINER_NAME=""
HOST_PORT=8888

# Parse options with getopts
while getopts "i:v:c:p:h" opt; do
  case $opt in
    i)
      IMAGE_NAME="$OPTARG"
      ;;
    v)
      IMAGE_VERSION="$OPTARG"
      ;;
    c)
      CONTAINER_NAME="$OPTARG"
      ;;
    p)
      HOST_PORT="$OPTARG"
      ;;
    h)
      display_help
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      display_help
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      display_help
      ;;
  esac
done

# If no custom container name is provided, generate a unique name
if [ -z "$CONTAINER_NAME" ]; then
  CONTAINER_NAME=$IMAGE_NAME
fi

IMAGE_VERSION=$(echo "$IMAGE_VERSION" | tr '[a-z]' '[A-Z]')
if [[ ${IMAGE_VERSION} == "AL5" ]];
then
  IMAGE_TAG=$(docker images --format "{{.Tag}}" --filter "reference=${IMAGE_NAME}" | grep -Ei al-5)
elif [[ ${IMAGE_VERSION} == "AL4" ]];
then
  IMAGE_TAG=$(docker images --format "{{.Tag}}" --filter "reference=${IMAGE_NAME}" | grep -Ei al-4)
else
  echo "Unsupported AL version. Supported AL5/AL4"
  exit 1
fi

xhost +local:
if [[ $(docker container ls -a | grep "$CONTAINER_NAME") ]];
then
  if [[ $(docker container inspect -f '{{.State.Status}}' $CONTAINER_NAME) == "exited" ]];
  then
    echo "starting container which was Exited"
    docker container start -i $CONTAINER_NAME
  else
    echo "Container is already running"
    exit 0
  fi
else
  echo "Starting fresh container"
  # The rest of the script remains the same
  docker run --name $CONTAINER_NAME \
           -e DISPLAY=$DISPLAY \
           -p ${HOST_PORT}:8888 \
           -v /tmp/.X11-unix:/tmp/.X11-unix:rw \
           -it "$IMAGE_NAME:$IMAGE_TAG"
#           -v /home/afilipczak/workspace/iWRAP/iwrap/docs/_build:/docs/_build \
fi

