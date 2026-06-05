# Set up environment
source ci/muscle3/st00-defs.sh
echo "executing $(basename "$0")"
export IWRAP_HOME=$(realpath "$(dirname ${BASH_SOURCE})/../..")

export PATH=${IWRAP_HOME}/bin:${PATH}

# Ensure build tools are available, then install iWrap (deps come from EasyBuild modules)
pip install --quiet setuptools setuptools_scm
pip install -e ${IWRAP_HOME} --no-deps -q

echo "IWRAP_HOME: $IWRAP_HOME"
echo "PATH: $PATH"

echo "IWRAP setup completed successfully"

