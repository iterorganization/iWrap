#######################################################################################################################
Configuration of the Test Environment
#######################################################################################################################

The test environment, which consists of a set of modules to be loaded for proper test execution, can be configured according to:

* The site where tests are run
* The compiler vendor to be used
* The Access Layer version

The configuration mechanism allows setting up the test environment on the ITER Organization SDCC cluster or the EUROfusion Gateway, for GCC or Intel toolchains and IMAS Access Layer version 5 or 4.

Test Requirements
================================================================================

The following libraries/components are essential to run the tests:

* IMAS Access Layer:

  * `AL Core` and `Python High Level Interface` to run iWrap
  * `AL Fortran/C++ High Level Interfaces` to test wrapping of the corresponding programming languages
  * `AL Java High Level Interface` and `JPype` to test Java wrapping

* `XMLlib` - to test code parameters stored in XML format
* `json-fortran` and `JsonCpp` - to test the JSON format of code parameters
* `f90nml` - to test the Fortran Namelist format of code parameters

The tests also require the following environment variables to be defined:

* For the C++ code wrapping tests:

  * `CXX` - C++ compiler (e.g., `g++`)
  * `MPICXX` - C++ MPI compiler script (e.g., `mpicxx`)

* For the Fortran code wrapping tests:

  * `FC` - Fortran compiler (e.g., `gfortran`)
  * `MPIFC` - Fortran MPI compiler script (e.g., `mpifort`)

The Structure of Configuration Files
================================================================================

The configuration files are stored in a tree-like structure in the `./envs` folder. The tree levels are:

* The **first** level refers to the site
* The **second** level refers to the compiler vendor
* The **third** level refers to the Access Layer version

The currently available configuration structure looks as follows:

.. code-block:: shell

    |-- gw
    |   |-- gcc
    |   |   |-- al4
    |   |   |   `-- configure_env.sh
    |   |   `-- al5
    |   |       `-- configure_env.sh
    |   `-- intel
    |       |-- al4
    |       |   `-- configure_env.sh
    |       `-- al5
    |           `-- configure_env.sh
    `-- iter
        |-- gcc
        |   |-- al4
        |   |   `-- configure_env.sh
        |   `-- al5
        |       `-- configure_env.sh
        `-- intel
            |-- al4
            |   `-- configure_env.sh
            `-- al5
                `-- configure_env.sh

Discovering the Proper Configuration Script
================================================================================

The configuration file (named `configure_env.sh`) is `sourced` in the `run-tests.sh` script by simply calling:

.. code-block:: shell

   shell> source ${IWRAP_HOME}/envs/${current_site}/${compiler_vendor}/al${al_version}/configure_env.sh

If the path cannot be found for the given combination of site, vendor, and Access Layer version (`al_version`), `run-tests.sh` exits with an error message.

Adding New Configuration
================================================================================

Extending the available configurations is quite simple. Depending on what needs to be configured, you should add the appropriate level of the 'configuration tree'.

.. admonition:: Example of the configuration file

    # IMAS DD: 3.42.0 / AL: 5.x / toolchain: GCC (gfbf-2023)

    module load IMAS/3.42.0-2024.08.1-foss-2023b

    module load lxml/4.9.3-GCCcore-13.2.0
    module load XMLlib/3.3.2-GCC-13.2.0
    module load JPype/1.5.0-gfbf-2023b
    module load json-fortran/8.5.2-GCC-13.2.0
    module load JsonCpp/1.9.5-GCCcore-13.2.0
    module load f90nml/1.4.4-GCCcore-13.2.0

    export CXX="g++"
    export FC="gfortran"
    export MPICXX="mpicxx"
    export MPIFC="mpifort"
