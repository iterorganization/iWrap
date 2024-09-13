#######################################################################################################################
Integration tests
#######################################################################################################################

Introduction
############

:download:`../../../iwrap/iwrap_main.py`
/gss_efgw_work/work/g2bpalak/iwrap/docs/documentation/developers_manual/09_integration_tests.rst
/gss_efgw_work/work/g2bpalak/iwrap/iwrap/iwrap_main.py

`here <file://../../../iwrap/iwrap_main.py>`_

`Link to file <../../../iwrap/iwrap_main.py>`_


`Link to file <./iwrap_main.html>`_

Test launcher
#######################################################################################################################

Defining a test case
#######################################################################################################################


Makefile
================
The test launching script calls target `test` from the Makefile.


Tests outcome
#######################################################################################################################

Every test creates its own directory under the folder `tests/reports` to keep all outputs. The directory name combines
the name of the test case and tested: code language, code parameters format and run mode
(`<test_case>_<lang>_<format>_<run_mode>`, eg. `13_code_lifecycle_parametrizable_cpp_xml_normal`).

Data logged for every test case include:

* Standard output, logged to `stdout.txt`
* Standard error, logged to `stderr.txt`
* Test return status, saved to `exit_code.txt` file

Additionally, info about all FAILED tests is stored in a file `tests/reports/failed_tests.log`

Tests post-processing
#######################################################################################################################
Script `tests/generate_junit_report` can be used to create 'JUnit like' report for Atlassian Bamboo CI/CD tests.
The script creates XML file (`reports/report.xml`) with a total number of tests run and separate entries for every failed test.




.. code-block:: python

    # iwrap.settings.code_parameters_handlers.handler_factory
    ...
    from .ZZXZ


.. note::
    BXXX
