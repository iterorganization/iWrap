#######################################################################################################################
Tests Usage
#######################################################################################################################

Launching Tests
#######################################################################################################################

Tests can be launched using the highly configurable `run-tests.sh` script.

.. code-block:: shell

    shell> ./run-tests.sh -h
    Usage:
         ./run-tests.sh [--language|-l <language>] [--test-case|-t <test name>] [--compiler|-c <vendor>] [--run-mode|-m <mode>] [--parameters-format|-f <format>] [--help|-h]

    optional arguments:
         -l, --language <language>          Test the specified language only. Available values: 'dummy fortran cpp java'
         -t, --test-case <test name>        Run a specific test case. Default: all values
         -c, --compiler <vendor>            Specify the compiler vendor. Available values: 'gcc intel'. Default: 'gcc'
         -m, --run-mode <mode>              Define the actor run mode. Available values: 'normal standalone'. Default: all values
         -f, --parameters-format <format>   Specify the code parameters formats to be tested. Available values: 'none legacy-xml xml namelist yaml json'. Default: all values
         -s, --test-site <site>             Site where tests are run. Available values: 'iter gw'. Default: 'iter'
         -a, --al-version <version>         IMAS Access Layer version. Available values: '4 5'. Default: '5'
         -h, --help                         Show this help message and exit

.. admonition:: **EXAMPLES:**

    * Running all the tests on the SDCC cluster for all languages, compilers, run modes, formats, etc.

    .. code-block:: shell

        shell> ./run-tests.sh

    * Running all the tests on the SDCC cluster for Access Layer 4.x and Intel compilers

    .. code-block:: shell

        shell> ./run-tests.sh --al-version 4 --compiler intel
           OR
        shell> ./run-tests.sh -a 4 -c intel

    * Running a single test on the SDCC cluster for Fortran (GCC)

    .. code-block:: shell

        # <test name> is the name of one of the test cases (subdirectories of the `./tests/test-cases` folder)

        shell> ./run-tests.sh --test-case <test name> --language fortran
           OR
        shell> ./run-tests.sh -t <test name> -l fortran

The script discovers all test cases and iterates over all tested languages and test parameter formats (unless the tester narrows down
the test scope using script command-line options). If the configuration of a particular test confirms that it can be run
for the given language/parameter format, it is launched.

Tests Outcome
================================================================================

Each test creates its own directory under the folder `tests/reports` to store all outputs. The directory name combines
the test case name and the tested code language, code parameter format, and run mode
(`<test_case>_<lang>_<format>_<run_mode>`, e.g., `13_code_lifecycle_parametrizable_cpp_xml_normal`).

Data logged for every test case includes:

* Standard output, logged to `stdout.txt`
* Standard error, logged to `stderr.txt`
* Test return status, saved to `exit_code.txt` file

Additionally, information about all FAILED tests is stored in the file `tests/reports/failed_tests.log`

Tests Post-Processing
================================================================================
The script `tests/generate_junit_report` can be used to create a 'JUnit-like' report for Atlassian Bamboo CI/CD tests.
This script generates an XML file (`reports/report.xml`) containing the total number of tests run and separate entries for each failed test.
