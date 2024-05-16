#!/bin/bash

# Function to display help information
function display_help {
    echo "Usage: $0 [options]"
    echo "Options:"
    echo "    -v <AL_VERSION>      Set the AL_VERSION | options are 'al4' or 'al5' | default is 'al5'"
    echo "    -t <DOCKER_TARGET>   Set the DOCKER_TARGET | options are 'book' or 'tutorial' | default is 'tutorial'"
    echo "    -i <IMAGE_NAME>      Set the IMAGE_NAME | default is 'iwrap_tutorial'"
    echo "    -c <CONTEXT>         Set the CONTEXT | default is '.'"
    echo "    -h                   Display this help message"
    exit 1
}

# Initialize variables with updated default values
BASE_IMAGE="gitlab.eufus.psnc.pl:5050/containerization/imas/imas-installer/al:DD-3.39.0_AL-5.0.0"
CONTEXT="."
DOCKER_TARGET="tutorial"
TARGET=""
JUPYTER_VER=""
IMAGE_NAME="iwrap_tutorial"
AL_VERSION="al5"

# Parse command-line arguments
while getopts "n:v:t:i:c:h" opt; do
    case ${opt} in
        v)
            AL_VERSION=${OPTARG}
            ;;
        t)
            DOCKER_TARGET=${OPTARG}
            ;;
        i)
            IMAGE_NAME=${OPTARG}
            ;;
        c)
            CONTEXT=${OPTARG}
            ;;
        h)
            display_help
            ;;
        *)
            echo "Invalid option: -$OPTARG"
            display_help
            ;;
    esac
done

# Configure BASE_IMAGE based on AL_VERSION
case ${AL_VERSION} in
    "al5")
        BASE_IMAGE='gitlab.eufus.psnc.pl:5050/containerization/imas/imas-installer/al:DD-3.39.0_AL-5.0.0'
        ;;
    "al4")
        BASE_IMAGE='gitlab.eufus.psnc.pl:5050/containerization/imas/imas-installer/al:DD-3.38.1_AL-4.11.7'
        ;;
    *)
        echo "Invalid AL_VERSION: ${AL_VERSION}"
        exit 1
        ;;
esac

# Get Tutorial image tag, based on imas-installer image
IMAGE_TAG=$(echo "$BASE_IMAGE" | sed -n 's/.*\(AL-.*\)/\1/p')

# Configure TARGET and JUPYTER_VER based on DOCKER_TARGET
case ${DOCKER_TARGET} in
    "book")
        TARGET="jupyter_book"
        ;;
    "tutorial")
        TARGET="jupyter_tutorial"
        ;;
    *)
        echo "Invalid DOCKER_TARGET: ${DOCKER_TARGET}"
        exit 1
        ;;
esac

# Build the Docker image
docker build \
    --build-arg BASE=${BASE_IMAGE} \
    --target ${TARGET} \
    -t ${IMAGE_NAME}:${IMAGE_TAG} ${CONTEXT}
