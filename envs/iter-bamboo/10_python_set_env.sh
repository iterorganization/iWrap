echo "--------------Module load GCC/9.3.0--------------"
module load GCC
echo "--------------Module load Python/3.8.2-GCCcore-9.3.0--------------"
module load Python
echo "--------------Module load PyYAML--------------"
module load PyYAML
echo "--------------Module load lxml/4.5.2-GCCcore-9.3.0--------------"
module load lxml
echo "--------------Module load pytest/5.4.3-GCCcore-9.3.0-Python-3.8.2--------------"
if module avail 2>&1 | grep -c pytest; then
  module load pytest
else
  python -m pip install pytest
fi
echo "--------------PIP install pylint + pylint-junit"
python -m pip install pylint
python -m pip install pylint-junit
echo "--------------Report Python and pytest version--------------"
touch python_report.txt
echo "PYTHON VERSION:" &> python_report.txt
python --version &>> python_report.txt
echo "PIP LIST:" &>> python_report.txt
python -m pip list &>> python_report.txt
echo "=================================END============================" &>> python_report.txt