.. sectnum::

.. toctree::

Installation
#######################################################################################################################

.. note::
   As of iWrap version 0.8.0, MUSCLE3 plugins have been integrated into the main iWrap package.
   The separate ``iwrap-plugins-muscle3`` repository has been archived.

MUSCLE3 support is now included in the main iWrap package and can be installed using pip with the ``muscle3`` extra.

Requirements
#######################################################################################################################

Software required to use MUSCLE3 plugins:

- `Python` >= 3.6
- `iWrap` >= 0.7.0
- `MUSCLE3` >= 0.7.0
- `IMAS` Access Layer >= 4.11 (for IMAS-based codes)

Software required to build documentation:

- `make` >= 3.82
- `Python` >= 3.8.6
- Python packages:

  - `Sphinx` >= 3.2.1
  - `sphinx-bootstrap-theme` >= 0.7.1
  - `sphinx-rtd-theme` >= 1.0.0

Installation via pip
#######################################################################################################################

Standard Installation
=========================================================================================

To install iWrap with MUSCLE3 support:

.. code-block:: console

    pip install iwrap[muscle3]

This will install:

- iWrap core package
- MUSCLE3 actor generators (Python, C++, Fortran)
- MUSCLE3 dependencies (muscle3, ymmsl)

Development Installation
=========================================================================================

For development, install in editable mode:

.. code-block:: console

    git clone https://git.iter.org/projects/IMEX/repos/iwrap
    cd iwrap
    pip install -e .[muscle3]

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
        MUSCLE3-CPP      :         MUSCLE3 (C++)          : Wrapping C++ code into MUSCLE3 micro model
      MUSCLE3-Fortran    :       MUSCLE3 (Fortran)        : Wrapping Fortran code into MUSCLE3 micro model

If the MUSCLE3 actor types are listed, the installation was successful.

Verify Python Imports
=========================================================================================

You can also verify that the MUSCLE3 generators can be imported:

.. code-block:: python

    from iwrap.generators.actor_generators.muscle3_python import M3PythonActor
    from iwrap.generators.actor_generators.muscle3_cpp import M3CppActor
    from iwrap.generators.actor_generators.muscle3_fortran import M3FortranActor
    from iwrap.generators.actor_generators.muscle3_common import m3_utils

If these imports succeed without errors, the MUSCLE3 plugins are correctly installed.

Environment Setup
#######################################################################################################################

ITER SDCC
=========================================================================================

For the ITER Organisation computing cluster (SDCC), use the provided configuration script:

.. code-block:: console

    source set-iter.sh

This script will:

- Load required modules (iWrap, MUSCLE3, IMAS)
- Set up environment variables
- Configure PYTHONPATH

EUROfusion Gateway
=========================================================================================

For the EUROfusion Gateway, use:

.. code-block:: console

    source set-gw.sh

This script provides similar functionality for the Gateway environment.

Manual Environment Setup
=========================================================================================

If you're not using the provided scripts, ensure the following are available:

1. **MUSCLE3 Libraries**: Available via pkg-config

   .. code-block:: console

      pkg-config --modversion muscle3

2. **IMAS Access Layer**: Properly configured

   .. code-block:: console

      module load imas

3. **Python Path**: iWrap should be in your PYTHONPATH (automatically handled by pip install)

Migration from Separate Plugin Package
#######################################################################################################################

If you were previously using the separate ``iwrap-plugins-muscle3`` package:

Old Installation
=========================================================================================

.. code-block:: console

    pip install iwrap
    pip install iwrap-plugins-muscle3

New Installation
=========================================================================================

.. code-block:: console

    pip install iwrap[muscle3]

Import Path Changes
=========================================================================================

Update your code to use the new import paths:

**Old (separate plugin):**

.. code-block:: python

    from iwrap_plugins.iwrap_actor_generator.muscle3_python import M3PythonActor

**New (integrated):**

.. code-block:: python

    from iwrap.generators.actor_generators.muscle3_python import M3PythonActor

See the :doc:`../muscle3_migration_guide` for more details on migrating from the separate plugin package.

Uninstall
#######################################################################################################################

To uninstall iWrap:

.. code-block:: console

   pip uninstall iwrap

This will remove both the core iWrap package and the integrated MUSCLE3 plugins.

Troubleshooting
#######################################################################################################################

MUSCLE3 Actor Types Not Listed
=========================================================================================

If ``iwrap --list-actor-types`` doesn't show MUSCLE3 actor types:

1. Verify MUSCLE3 extra was installed:

   .. code-block:: console

      pip show iwrap | grep muscle3

2. Reinstall with MUSCLE3 support:

   .. code-block:: console

      pip install --force-reinstall iwrap[muscle3]

Import Errors
=========================================================================================

If you get import errors when trying to use MUSCLE3 generators:

1. Verify iWrap is installed:

   .. code-block:: console

      python -c "import iwrap; print(iwrap.__version__)"

2. Verify MUSCLE3 dependencies are installed:

   .. code-block:: console

      python -c "import muscle3; print(muscle3.__version__)"

3. Check your Python environment is correct:

   .. code-block:: console

      which python
      which iwrap

MUSCLE3 Library Not Found
=========================================================================================

If you get errors about MUSCLE3 libraries not being found during actor compilation:

1. Verify MUSCLE3 is available via pkg-config:

   .. code-block:: console

      pkg-config --modversion muscle3
      pkg-config --cflags muscle3
      pkg-config --libs muscle3

2. Load the MUSCLE3 module (if using environment modules):

   .. code-block:: console

      module load muscle3

3. Check the MUSCLE3 installation documentation for your platform.

