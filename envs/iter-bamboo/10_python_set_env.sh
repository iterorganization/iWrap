echo "--------------Module load GCCcore/10.2.0--------------"
module load GCCcore/10.2.0
echo "--------------Module load Python/3.8.6-GCCcore-10.2.0--------------"
module load Python/3.8.6-GCCcore-10.2.0
echo "--------------Module load Tkinter/3.8.6-GCCcore-10.2.0--------------"
module load Tkinter/3.8.6-GCCcore-10.2.0
echo "--------------Module load PyYAML/5.3.1-GCCcore-10.2.0--------------"
module load PyYAML/5.3.1-GCCcore-10.2.0
echo "--------------Module load lxml--------------"
module load lxml
echo "--------------Module load pytest--------------"
module load pytest
echo "--------------PIP UPGRADE--------------"
python -m pip install --upgrade pip --disable-pip-version-check --no-warn-script-location
echo "--------------PIP install pylint + pylint-junit--------------"
python -m pip install pylint --disable-pip-version-check --no-warn-script-location
python -m pip install pylint-junit --disable-pip-version-check --no-warn-script-location
echo "--------------Report Python and pytest version--------------"
touch python_report.txt
echo "PYTHON VERSION:" &> python_report.txt
python --version &>> python_report.txt
echo "PIP LIST:" &>> python_report.txt
python -m pip list &>> python_report.txt
echo "=================================END============================" &>> python_report.txt