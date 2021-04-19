echo "~~~~~====================PYLINT CODE CHECK====================~~~~~"
python -m pylint -E --output-format=pylint_junit.JUnitReporter iwrap > pylint.xml