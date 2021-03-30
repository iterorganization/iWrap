clear
set -a
# Prepare tests
echo "|--------------Prepare For Tests--------------|"
# Verify whether script is running on CI server
# Checks if $ONBAMBOO exists in environment
if [ -z "${ONBAMBOO+x}" ]
then
  echo "!!!--------------NOTBAMBOO--------------!!!"
else
  echo "!!!--------------ONBAMBOO--------------!!!"
  # Source and run script with environment vars for CI server
  . ./set_test_env.sh
fi
# Run Pytests
echo "|--------------Run Tests--------------|"
python --version
python -m pytest --version
# Runs python pytest and logs results to junit xml file
python -m pytest example_test.py --junitxml=test_results.xml -v


