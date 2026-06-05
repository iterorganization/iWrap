# Set up environment
source ci/muscle3/st00-defs.sh
echo "executing $(basename "$0")"
export IWRAP_HOME=$(realpath "$(dirname ${BASH_SOURCE})/../..")

# Ensure we are inside a virtual env before pip installing
if [ -z "$VIRTUAL_ENV" ]; then
    python -m venv --system-site-packages ${IWRAP_HOME}/.venv
    . ${IWRAP_HOME}/.venv/bin/activate
fi
pip install --quiet setuptools setuptools_scm
pip install -e ${IWRAP_HOME} --no-deps -q

echo "IWRAP_HOME: $IWRAP_HOME"
echo "PATH: $PATH"

echo "IWRAP setup completed successfully"

