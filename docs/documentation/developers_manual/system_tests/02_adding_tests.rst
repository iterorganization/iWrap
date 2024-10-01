#######################################################################################################################
Defining a New Test Case
#######################################################################################################################

This chapter describes how to define and add a new test case. Developers have considerable freedom
in designing tests—virtually anything that can be run from the command line can be tested.
However, tests usually focus on checking the actor generation process or actor API functionality and require:

* The code to be wrapped
* A description of the actor to be generated
* The test case definition

How to Add New Code to Be Wrapped
#######################################################################################################################
The new code must implement (a part of or all of) the 'iWrap standard' methods:
*INIT*, *MAIN*, *FINALIZE*, *GET_STATE*, *SET_STATE*, and *GET_TIMESTAMP* (see the :ref:`code_api` chapter for details).
The new code should be added to the subdirectory of `test/codes` corresponding to its programming language (`cpp`/`fortran`/`java`).
Every code must have a dedicated `Makefile` responsible for compiling the code and creating
a library (C++, Fortran) or archive (Java) that can be used by iWrap for actor generation.

.. admonition:: **Tricks for Code Reusability:**

   * To reduce the number of code lines, use method overloading where possible.
   * A single test case may check several codes written in various languages if the codes share the same directory name
     and method APIs (i.e., method names and their arguments).
   * The given parameterized code should recognize and accept various formats of the code parameters.

How to Add a New Actor
#######################################################################################################################
The YAML file containing essential information to properly wrap the code and generate an actor (see :ref:`here<yaml_project_description_anchor>`)
should be placed in the folder `tests/actors` in a subdirectory corresponding to the programming language of the code.

.. admonition:: **Tricks for Code Reusability**

   To apply the same description file for several tests:

   * System variables (e.g., for defining code parameter formats or compiler commands) can be used.
   * Descriptions of code written in various languages but having the same API (method names, argument types, and numbers)
     can be saved in directories and YAML files of the same name.

Putting Elements Together: The Test Case Definition
#######################################################################################################################
To add a new test, simply create a new directory within the `tests/test_cases` folder. It requires no manual 'registration' as
tests are automatically discovered by the test launcher. The simplest (minimal) test case requires only two **mandatory** elements:

* A `Makefile` with the target `test`
* A configuration file `test.cfg`, specifying a list of programming languages for which the test can be run

So, the simplest test case could be designed as follows:

.. code-block:: shell
   :caption: The test directory minimal content

    test-cases
    |
    |-- the_simplest_test_case
    |   |-- Makefile
    |   `-- test.cfg

.. code-block:: shell
   :caption: Makefile

    # The mandatory target **test**
    test:
        true

.. code-block:: shell
   :caption: test.cfg

    # Defines what languages can be checked with this test case
    # Undefined or empty = no language (i.e., test without wrapping the codes)
    TEST_LANGUAGES=

Of course, a test case prepared in such a way does nothing and makes no sense...
The next chapter will show how actual test cases are defined.

Advanced Test Cases
===============================================================================
The directory `tests/test_cases` contains several system tests stored in subdirectories.
Tests that check the correctness of code wrapping and actor generation require:

* The code to be wrapped
* A description of the actor to be generated
* The test case definition

Every test consists of the following files:

* `expected.out` - contains the expected results of the test.
* `test.cfg` - stores system variables that define the test scope (`$TEST_LANGUAGES`), the name of the test script (`$TEST_SCRIPT`),
  and describe the tested actor (`$ACTOR_NAME`, `$CODE_LANGUAGE`, `$PARAMETERS_FORMAT`)
* A Python script that performs some test operations on the actor (calls the wrapped code or other methods of the actor API)
* `Makefile`

The test launching script calls the target `test` from the Makefile. This target depends on sequentially called targets:

* `clean` - removes artifacts of previous runs
* `code` - builds the code to be wrapped
* `actor` - uses iWrap to wrap the code and generate the actor. The generated actors are installed in
* `wf-run` - runs a Python script that tests the actor created in the previous step
* `check-output` - compares expected test results (`expected.out`) against the real ones (`test.out`)

.. admonition:: **Tricks for Code Reusability**

   * The Makefile may define only the target `test` and include all other targets
     from `tests/test_cases/Makefile.common`. However, depending on the test purposes,
     any target can be 'redefined' in a local Makefile.
   * The shared Makefile reads information about the given actor from the configuration file (`test.cfg`).
   * The Python script uses the `utils.get_actor_instance()` method to initialize an object of the actor class.
     Because the actor package and class name postfixes depend on the language being wrapped and the code parameter format handled,
     the method creates the actor name dynamically. When the Python script is run by the test launcher (`run-tests.sh`),
     the method applies the system variables `$ACTOR_NAME`, `$CODE_LANGUAGE`, and `$PARAMETERS_FORMAT`. In other cases
     (e.g., manual run of the script), the method looks for its arguments:
     `utils.get_actor_instance(<actor_name>, <code_language>, <parameters_format>)`
