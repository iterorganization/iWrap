MUSCLE3 Plugins Documentation
==============================

This section provides detailed documentation for the MUSCLE3 actor generators
integrated into iWrap.

.. note::
   As of iWrap version 0.8.0, MUSCLE3 plugins have been integrated into the
   main iWrap package. The separate ``iwrap-plugins-muscle3`` repository has
   been archived.

Contents
--------

.. toctree::
   :maxdepth: 2

   installation
   code_wrapping
   examples

Overview
--------

The MUSCLE3 plugins provide actor generators for three programming languages:

* **Python** - ``iwrap.generators.actor_generators.muscle3_python``
* **C++** - ``iwrap.generators.actor_generators.muscle3_cpp``
* **Fortran** - ``iwrap.generators.actor_generators.muscle3_fortran``

These generators create MUSCLE3-compatible actors from your physics codes,
enabling multiscale coupling through the MUSCLE3 framework.

What are MUSCLE3 Actors?
-------------------------

MUSCLE3 (Multiscale Coupling Library and Environment 3) is a framework for
building multiscale coupled simulations. A MUSCLE3 actor is a component in
a coupled simulation that:

* Communicates with other actors through well-defined ports
* Exchanges data using the MUSCLE3 communication infrastructure
* Can be written in Python, C++, or Fortran
* Follows the MUSCLE3 API conventions

The iWrap MUSCLE3 plugins automate the process of wrapping your existing
physics codes into MUSCLE3 actors, handling:

* Port definitions and data exchange
* MUSCLE3 API integration
* Build system generation
* Parameter handling
* Restart/checkpoint support

Quick Start
-----------

1. **Install iWrap with MUSCLE3 support:**

   .. code-block:: bash

      pip install iwrap[muscle3]

2. **Prepare your code description YAML file:**

   Define your code's interface, parameters, and implementation details.
   See :doc:`code_wrapping` for detailed instructions.

3. **Generate a MUSCLE3 actor:**

   .. code-block:: bash

      iwrap --actor-type muscle3_python --file my_code.yaml --install-dir ./actor

   Replace ``muscle3_python`` with ``muscle3_cpp`` or ``muscle3_fortran`` as needed.

4. **Build the generated actor:**

   .. code-block:: bash

      cd actor
      make

5. **Run your coupled simulation:**

   Create a yMMSL workflow file and run with MUSCLE3:

   .. code-block:: bash

      muscle_manager --start-all my_workflow.ymmsl

For a complete working example, see :doc:`examples`.

Supported Languages
-------------------

Python
~~~~~~

**Actor Type:** ``muscle3_python``

**Features:**

* Pure Python implementation
* Easy to debug and modify
* Supports all MUSCLE3 features
* Ideal for prototyping and Python-based codes

**Import:**

.. code-block:: python

   from iwrap.generators.actor_generators.muscle3_python import M3PythonActor

C++
~~~

**Actor Type:** ``muscle3_cpp``

**Features:**

* High performance
* Direct integration with C++ physics codes
* Supports MPI parallelization
* Automatic build system generation

**Import:**

.. code-block:: python

   from iwrap.generators.actor_generators.muscle3_cpp import M3CppActor

Fortran
~~~~~~~

**Actor Type:** ``muscle3_fortran``

**Features:**

* Native Fortran support
* Optimized for HPC environments
* Supports MPI parallelization
* Compatible with legacy Fortran codes

**Import:**

.. code-block:: python

   from iwrap.generators.actor_generators.muscle3_fortran import M3FortranActor

Common Utilities
~~~~~~~~~~~~~~~~

All generators share common utilities for MUSCLE3 integration:

.. code-block:: python

   from iwrap.generators.actor_generators.muscle3_common import m3_utils

Key Features
------------

Automatic Code Generation
~~~~~~~~~~~~~~~~~~~~~~~~~

The plugins automatically generate:

* MUSCLE3 actor wrapper code
* Build system (Makefile)
* Port definitions and data exchange logic
* Parameter handling
* Initialization and finalization code

IMAS Integration
~~~~~~~~~~~~~~~~

Full support for IMAS (Integrated Modelling & Analysis Suite):

* Automatic IDS (Interface Data Structure) handling
* Data conversion between IMAS and MUSCLE3 formats
* Support for IMAS Access Layer

MPI Support
~~~~~~~~~~~

For parallel codes:

* MPI initialization and finalization
* Rank-aware data distribution
* Collective operations support

Restart/Checkpoint
~~~~~~~~~~~~~~~~~~

Built-in support for:

* Saving simulation state
* Restarting from checkpoints
* Snapshot management

See Also
--------

* :doc:`../muscle3_actors` - Overview of MUSCLE3 actors in iWrap
* :doc:`../muscle3_migration_guide` - Migration guide for existing users
* :doc:`installation` - Installation instructions
* :doc:`code_wrapping` - Detailed code wrapping guide
* :doc:`examples` - Complete working examples

External Resources
------------------

* `MUSCLE3 Documentation <https://muscle3.readthedocs.io/>`_
* `IMAS Documentation <https://imas.iter.org/>`_
* `iWrap Repository <https://git.iter.org/projects/IMEX/repos/iwrap>`_
