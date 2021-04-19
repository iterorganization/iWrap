echo "--------------Module load GCC/9.3.0--------------"
module load GCC/9.3.0
echo "--------------Module load Python/3.8.2-GCCcore-9.3.0--------------"
module load Python/3.8.2-GCCcore-9.3.0
echo "--------------Module load PyYAML/5.3-GCCcore-9.3.0--------------"
module load PyYAML/5.3-GCCcore-9.3.0
echo "--------------Module load lxml/4.5.2-GCCcore-9.3.0--------------"
module load lxml/4.5.2-GCCcore-9.3.0
echo "--------------Module load pytest/5.4.3-GCCcore-9.3.0-Python-3.8.2--------------"
module load pytest/5.4.3-GCCcore-9.3.0-Python-3.8.2
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