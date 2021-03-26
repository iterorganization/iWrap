clear
# Prepare tests
echo "---Prepare For Tests---"
#ONBAMBOO=1
if [ -z "${ONBAMBOO+x}" ]
then
  echo "NOTBAMBOO"
else
  echo "ONBAMBOO"
  #./set_test_env.sh
fi
# Run Pytests
echo "---Run Tests---"
python --version
pytest --version
python -m pytest example_test.py --junitxml=test_results.xml -v


