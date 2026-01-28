MUSCLE3 Examples
================

This section provides complete working examples of MUSCLE3 actor generation
and usage with iWrap.

Basic Fortran Example
---------------------

This example demonstrates wrapping a simple Fortran code as a MUSCLE3 actor.
The example shows a complete workflow from code description to running a
coupled simulation.

Example Files
~~~~~~~~~~~~~

All example files are located in ``docs/documentation/muscle3_plugins/example/``:

* ``code_description.yaml`` - Actor description for iWrap
* ``example.ymmsl`` - MUSCLE3 workflow configuration
* ``macro.f90`` - Macro model (orchestrator)
* ``standalone.f90`` - Original standalone code
* ``wrapped_code.f90`` - Code prepared for wrapping
* ``Makefile`` - Build instructions

Code Description
~~~~~~~~~~~~~~~~

The code description YAML file defines the actor interface:

.. literalinclude:: example/code_description.yaml
   :language: yaml
   :caption: code_description.yaml

The code description defines:

* **Actor name and type**: Identifies the actor and generator to use
* **Input/output ports**: Defines data exchange interfaces
* **Code parameters**: Configuration options for the wrapped code
* **Implementation details**: Language, source files, build options

Workflow Configuration
~~~~~~~~~~~~~~~~~~~~~~

The yMMSL file defines the coupled simulation workflow:

.. literalinclude:: example/example.ymmsl
   :language: yaml
   :caption: example.ymmsl

The yMMSL file defines:

* **Workflow components**: Macro and micro models
* **Port connections**: How actors exchange data
* **Simulation parameters**: Runtime configuration
* **Resources**: Computational resources for each component

Wrapped Code
~~~~~~~~~~~~

The Fortran code to be wrapped:

.. literalinclude:: example/wrapped_code.f90
   :language: fortran
   :caption: wrapped_code.f90

This code implements the physics model that will be wrapped as a MUSCLE3 actor.

Macro Model
~~~~~~~~~~~

The macro model orchestrates the coupled simulation:

.. literalinclude:: example/macro.f90
   :language: fortran
   :caption: macro.f90

The macro model:

* Initializes the simulation
* Sends data to the micro model (wrapped code)
* Receives results from the micro model
* Manages the simulation loop

Building the Example
~~~~~~~~~~~~~~~~~~~~

Step 1: Generate the MUSCLE3 Actor
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   cd docs/documentation/muscle3_plugins/example
   
   # Generate the MUSCLE3 actor
   iwrap --actor-type muscle3_fortran \
         --file code_description.yaml \
         --install-dir ./m3_actor

This command:

* Reads the code description
* Generates MUSCLE3 wrapper code
* Creates build system (Makefile)
* Sets up the actor directory structure

Step 2: Build the Actor
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   cd m3_actor
   make

This compiles:

* The wrapped physics code
* The MUSCLE3 wrapper
* All dependencies

Step 3: Build the Macro Model
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   cd ../
   make macro

This compiles the macro model that will orchestrate the simulation.

Step 4: Run the Coupled Simulation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   muscle_manager --start-all example.ymmsl

This:

* Starts the MUSCLE3 manager
* Launches both macro and micro models
* Manages data exchange
* Runs the coupled simulation

Expected Output
~~~~~~~~~~~~~~~

The simulation should produce output showing:

.. code-block:: text

   [MUSCLE3] Starting simulation...
   [Macro] Initializing macro model
   [Macro] Sending data to micro model
   [Micro] Received data from macro
   [Micro] Processing...
   [Micro] Sending results to macro
   [Macro] Received results from micro
   [MUSCLE3] Simulation complete

Output files will be created in a ``run_*`` directory containing:

* Simulation logs
* Output data
* Performance metrics
* Snapshots (if checkpointing enabled)

More Examples
-------------

The test suite contains additional examples for different scenarios:

Python Examples
~~~~~~~~~~~~~~~

Located in ``tests/muscle3/actors/python/``:

* **basic_python**: Simple Python actor
* **restart_python**: Python actor with restart support

C++ Examples
~~~~~~~~~~~~

Located in ``tests/muscle3/actors/cpp/``:

* **basic_cpp**: Simple C++ actor
* **basic_mpi_cpp**: MPI-parallel C++ actor
* **restart_cpp**: C++ actor with restart support
* **restart_mpi_cpp**: MPI-parallel C++ actor with restart

Fortran Examples
~~~~~~~~~~~~~~~~

Located in ``tests/muscle3/actors/fortran/``:

* **code_lifecycle**: Complete lifecycle example
* **basic_mpi**: MPI-parallel Fortran actor
* **code_restart**: Fortran actor with restart support
* **code_restart_mpi**: MPI-parallel Fortran actor with restart

Running Test Examples
~~~~~~~~~~~~~~~~~~~~~

Each test example can be run using its Makefile:

.. code-block:: bash

   cd tests/muscle3/actors/<language>/<example_name>
   make test

This will:

1. Build the wrapped code
2. Generate the MUSCLE3 actor
3. Run the coupled simulation
4. Validate the output

Example: Running the Python Basic Test
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   cd tests/muscle3/actors/python/basic_python
   make test

Advanced Features
-----------------

MPI Parallelization
~~~~~~~~~~~~~~~~~~~

For parallel codes, the MUSCLE3 generators support MPI:

.. code-block:: yaml

   # In code_description.yaml
   implementation:
     language: fortran
     parallel: mpi
     mpi_ranks: 4

See the ``basic_mpi`` examples for complete implementations.

Restart/Checkpoint Support
~~~~~~~~~~~~~~~~~~~~~~~~~~

Enable restart support in your code description:

.. code-block:: yaml

   # In code_description.yaml
   features:
     restart: true
     checkpoint_interval: 10

See the ``restart_*`` examples for complete implementations.

Custom Parameters
~~~~~~~~~~~~~~~~~

Define custom parameters for your code:

.. code-block:: yaml

   # In code_description.yaml
   parameters:
     - name: timestep
       type: float
       default: 0.01
     - name: max_iterations
       type: integer
       default: 100

IMAS Integration
~~~~~~~~~~~~~~~~

For IMAS-based codes, specify IDS usage:

.. code-block:: yaml

   # In code_description.yaml
   ports:
     input:
       - name: equilibrium_in
         ids: equilibrium
     output:
       - name: equilibrium_out
         ids: equilibrium

Troubleshooting
---------------

Actor Generation Fails
~~~~~~~~~~~~~~~~~~~~~~~

If ``iwrap`` fails to generate the actor:

1. Check the code description YAML syntax
2. Verify all required fields are present
3. Check iWrap logs for detailed error messages
4. Ensure MUSCLE3 plugins are installed: ``iwrap --list-actor-types``

Build Fails
~~~~~~~~~~~

If the actor fails to build:

1. Verify MUSCLE3 libraries are available: ``pkg-config --modversion muscle3``
2. Check compiler is available: ``gfortran --version`` or ``g++ --version``
3. Review build logs in the actor directory
4. Ensure all source files are present

Runtime Errors
~~~~~~~~~~~~~~

If the simulation fails at runtime:

1. Check MUSCLE3 manager logs
2. Verify yMMSL workflow configuration
3. Ensure port connections are correct
4. Check for MPI configuration issues (if using MPI)

Next Steps
----------

* Adapt the basic example to your own code
* Explore advanced features in the :doc:`code_wrapping` guide
* Review the :doc:`../muscle3_actors` documentation
* Check the test suite for more examples
* Consult the `MUSCLE3 documentation <https://muscle3.readthedocs.io/>`_

See Also
--------

* :doc:`installation` - Installation instructions
* :doc:`code_wrapping` - Detailed code wrapping guide
* :doc:`../muscle3_actors` - Overview of MUSCLE3 actors
* :doc:`../muscle3_migration_guide` - Migration guide
