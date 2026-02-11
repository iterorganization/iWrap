#######################################################################################################################
Installation process
#######################################################################################################################

Installation
#######################################################################################################################

**1. Build an installable iWrap distribution:**

* Navigate to the project's root directory and run:

.. code-block:: shell

   make iwrap_build

**2. Prepare the installation directory path and the modulefile installation dir:**

* Installation directory:

.. code-block:: bash

   export INSTALL_DIR=[DIRECTORY...]

where DIRECTORY is the absolute path to the installation directory. 

A new folder will be created in the specified directory path with a name based on the current version tag.

*  Modulefile installation directory:

.. code-block:: bash

   export INSTALL_MOD=[DIRECTORY...]

where DIRECTORY is the absolute path to the directory where the module file should be placed.

A new subdirectory named as project will be created in this directory, 
and inside it a module file named after the current version tag will be placed.

**3. Install:**

* Run installation recipe:

.. code-block:: shell

   make install

If the installation is successful, the following information will be printed:

.. code-block:: bash

        IWRAP_INSTALL FINISHED
        iWrap installed in: [INSTALL_DIR/VERSION]  
    and
        INSTALL_MODULE FINISHED
        iWrap ENVIRONMENT MODULE installed in: [INSTALL_MOD/MODULEFILE]

After a successful installation, it is recommended to clean the root of the project from compilation residues. To do this, run a cleaning recipe:

.. code-block:: shell

   make clean

Extras
#######################################################################################################################

**1. Uninstall iWrap:**

Proper uninstallation of iWrap (including its distribution and module file) requires the use of the same INSTALL_DIR and INSTALL MOD variables as in the installation process.

* Run the uninstall recipe:

.. code-block:: shell
   
   export INSTALL_DIR=[DIRECTORY...]
   export INSTALL_MOD=[DIRECTORY...]
   
   make uninstall

**Remember that from the given directories only the current tagged version will be uninstalled!**

**2. Verification of the prepared installation configuration:**

After setting the INSTALL_DIR and INSTALL_MOD variables, the pre-installation configuration can be verified using the help recipe:

.. code-block:: shell
   
   export INSTALL_DIR=[DIRECTORY...]
   export INSTALL_MOD=[DIRECTORY...]
   
   make help

This will automatically evaluate all paths and print them with the appropriate descriptions.

**3. Adding the module file to the MODULEPATH variable:**

If the INSTALL_MOD directory is not already attached to MODULEPATH, it can simply be added manually:

.. code-block:: shell
   
   module use INSTALL_MOD

**4. Diagnostics:**

* After installing the module and adding it to MODULEPATH, check if it is available

.. code-block:: shell
   
   module avail iwrap

* If so, load the module into the environment:

.. code-block:: shell
   
   module load iwrap

* Check that iWrap has loaded properly and that the Python package manager is able to find it:

.. code-block:: shell
   
   python3 -m pip list

and look for iwrap package,

or:

.. code-block:: shell
   
   python3 -m pip list | grep iwrap

**5. Run iWrap:**

Load the module into the environment:

.. code-block:: shell
   
   module load iwrap

* Command line:

.. code-block:: shell
   
   iwrap

* GUI:

.. code-block:: shell
   
   iwrap-gui

iWrap Python Installation
#######################################################################################################################

MUSCLE3 support is now included in the main iWrap package and can be installed using pip with the ``muscle3`` extra.


Requirements
#######################################################################################################################

Software required to use MUSCLE3 plugins:

- `Python` 
- `iWrap` 
- `MUSCLE3`
- `IMAS-Fortran` (if using Fortran actors)
- `IMAS-Cpp` (if using C++ actors)
- `IMAS-Java` (if using Java actors)
- `XMLLib` (for XML parsing)

Software required to build documentation:

- `make` 
- `Python` 
- Python packages:

  - `Sphinx` 
  - `sphinx-bootstrap-theme` 
  - `sphinx-rtd-theme` 


`C and Fortran libmuscle installation <https://muscle3.readthedocs.io/en/latest/installing.html#c-and-fortran>`_

Overview
================

Install iWrap without MUSCLE3 support:

.. code-block:: bash

   pip install iwrap

This installs the basic iWrap functionality with the standard Python actor generator.

Installation with MUSCLE3 Support
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To enable MUSCLE3 actor generators, install with the MUSCLE3 extra:

.. code-block:: bash

   pip install iwrap[muscle3]

Or alternatively:

.. code-block:: bash

   pip install iwrap
   pip install -r requirements_muscle3.txt

This will install:

- iWrap core package
- MUSCLE3-Python actor generator
- MUSCLE3 dependencies (muscle3, ymmsl)

If you need MUSCLE3-Cpp or MUSCLE3-Fortran actors, install libmuscle separately:

`C and Fortran libmuscle installation <https://muscle3.readthedocs.io/en/latest/installing.html#c-and-fortran>`_


Development Installation
~~~~~~~~~~~~~~~~~~~~~~~~

For development, install in editable mode:

.. code-block:: bash

    git clone https://github.com/iterorganization/iWrap.git
    cd iwrap
    pip install -e .[muscle3]

Full Installation (All Features)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For development or to install all optional features:

.. code-block:: bash

   pip install iwrap[all]

This includes MUSCLE3 support and all other optional dependencies.

Installation from Source
#######################################################################################################################

Using Makefile
=========================================================================================

The iWrap repository includes a Makefile for building and installing:

.. code-block:: console

    make all                    # Build iWrap
    make install                # Install iWrap
    make install_module         # Install environment module

To check all available targets and configuration options:

.. code-block:: console

    shell> make help

Variables that can be configured:

- ``PYTHON_CMD`` - Python interpreter to use (default: python)
- ``INSTALL_PREFIX`` - Installation directory prefix
- ``INSTALL_MOD`` - Module file installation directory
- ``VERSION`` - iWrap version (auto-detected from git)

Verification
#######################################################################################################################

Verification of MUSCLE3 Plugins Installation
=========================================================================================

Once iWrap is installed with MUSCLE3 support, verify that the MUSCLE3 actor generators are available:

.. code-block:: console

    shell> iwrap --list-actor-types

             Id          :              Name              : Description
    ----------------------------------------------------------------------
           python        :      Simple Python actor       : Simple Python actor
       MUSCLE3-Python    :        MUSCLE3 (Python)        : Wrapping Python code into MUSCLE3 micro model

If the MUSCLE3 actor types are listed, the installation was successful.

Verify Python Imports
=========================================================================================

You can also verify that the MUSCLE3 generators can be imported:

.. code-block:: python

    from iwrap.generators.actor_generators.muscle3_python import M3PythonActor
    from iwrap.generators.actor_generators.muscle3_common import m3_utils

If these imports succeed without errors, the MUSCLE3 plugins are correctly installed.

Manual Environment Setup
========================

If you're not using the provided scripts, ensure the following are available:

1. **MUSCLE3 Libraries**: Available via pkg-config

   .. code-block:: bash

      pkg-config --modversion muscle3

      * libmuscle (CPP + Fortran) — `MUSCLE3 <https://github.com/multiscale/muscle3>`_ (if using MUSCLE3 actors)

2. **IMAS Access Layer**: Properly configured

   .. code-block:: bash

      module load IMAS-Matlab
      module load IMAS-Fortran
      module load IMAS-Cpp
      module load IMAS-Java
   
   See: `IMAS-Matlab <https://github.com/iterorganization/IMAS-Matlab>`_, 
   `IMAS-Fortran <https://github.com/iterorganization/IMAS-Fortran>`_, 
   `IMAS-Cpp <https://github.com/iterorganization/IMAS-Cpp>`_, 
   `IMAS-Java <https://github.com/iterorganization/IMAS-Java>`_
      
3. **Python Path**: iWrap should be in your PYTHONPATH (automatically handled by pip install)

4. **Extras**: Properly configured

   .. code-block:: bash

      module load XMLLib

Verifying Installation
~~~~~~~~~~~~~~~~~~~~~~

To verify which actor generators are available:

.. code-block:: bash

   iwrap --list-actor-types

Expected output with MUSCLE3 installed:

.. code-block:: text

            Id          :              Name              : Description
   ----------------------------------------------------------------------
           python        :      Simple Python actor       : Simple Python actor
       MUSCLE3-Python    :        MUSCLE3 (Python)        : Wrapping Python code into MUSCLE3 micro model

You can also verify that the MUSCLE3 generators can be imported:

.. code-block:: python

    from iwrap.generators.actor_generators.muscle3_python import M3PythonActor
    from iwrap.generators.actor_generators.muscle3_common import m3_utils

If these imports succeed without errors, the MUSCLE3 plugins are correctly installed.

Troubleshooting
#######################################################################################################################

MUSCLE3 Actor Types Not Listed
===============================

If ``iwrap --list-actor-types`` doesn't show MUSCLE3 actor types:

1. Verify MUSCLE3 extra was installed:

   .. code-block:: bash

      pip show iwrap | grep muscle3

2. Reinstall with MUSCLE3 support:

   .. code-block:: bash

      pip install --force-reinstall iwrap[muscle3]

Import Errors
=============

If you get import errors when trying to use MUSCLE3 generators:

1. Verify iWrap is installed:

   .. code-block:: bash

      python -c "import iwrap; print(iwrap.__version__)"

2. Verify MUSCLE3 dependencies are installed:

   .. code-block:: bash

      python -c "import muscle3; print(muscle3.__version__)"

3. Check your Python environment is correct:

   .. code-block:: bash

      which python
      which iwrap

MUSCLE3 Library Not Found
=========================

If you get errors about MUSCLE3 libraries not being found during actor compilation:

1. Verify MUSCLE3 is available via pkg-config:

   .. code-block:: bash

      pkg-config --modversion muscle3
      pkg-config --cflags muscle3
      pkg-config --libs muscle3

2. Load the MUSCLE3 module (if using environment modules):

   .. code-block:: bash

      module load muscle3

3. Check the MUSCLE3 installation documentation for your platform.


Migration from Separate Plugin Package
#######################################################################################################################

If you were previously using the separate ``iwrap-plugins-muscle3`` package:

Import Path Changes
=========================================================================================

Update your code to use the new import paths:

**Old (separate plugin):**

.. code-block:: python

    from iwrap_plugins.iwrap_actor_generator.muscle3_python import M3PythonActor

**New (integrated):**

.. code-block:: python

    from iwrap.generators.actor_generators.muscle3_python import M3PythonActor



