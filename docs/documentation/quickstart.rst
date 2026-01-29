========================================
Quick Start Guide
========================================

This guide will help you get started with iWrap quickly, covering both standard Python actors and MUSCLE3 actors.

Installation
========================================

Choose Your Installation
------------------------

**Option 1: Core Only (Minimal)**

.. code-block:: bash

   pip install iwrap

Includes: Standard Python actor generator

**Option 2: With MUSCLE3 Support (Recommended)**

.. code-block:: bash

   pip install iwrap[muscle3]

Includes: Python actor + MUSCLE3 actors (Python, C++, Fortran)

**Option 3: Development (All Features)**

.. code-block:: bash

   pip install iwrap[all]

Includes: All features and development tools

Verify Installation
-------------------

.. code-block:: bash

   iwrap --version
   iwrap --list-actor-types

Your First Actor (Standard Python)
========================================

Step 1: Prepare Your Code
--------------------------

Create a simple Python physics code ``my_code.py``:

.. code-block:: python

   # my_code.py
   def init():
       """Initialize the code"""
       print("Initializing...")
   
   def run(equilibrium_in, core_profiles_out):
       """Main computation"""
       print("Running step...")
       # Your physics calculations here
       return equilibrium_in, core_profiles_out
   
   def finalize():
       """Cleanup"""
       print("Finalizing...")

Step 2: Create Code Description
--------------------------------

Create ``code_description.yaml``:

.. code-block:: yaml

   actor_description:
     actor_name: my_first_actor
     actor_type: python
   
   code_description:
     implementation:
       programming_language: python
       code_path: ./my_code.py
       
       subroutines:
         init:
           name: init
           arguments: []
         
         step:
           name: run
           arguments:
             - name: equilibrium
               type: input
             - name: core_profiles
               type: output
         
         finalize:
           name: finalize
           arguments: []

Step 3: Generate Actor
-----------------------

.. code-block:: bash

   iwrap -a my_first_actor -t python -f code_description.yaml

Step 4: Use the Actor
----------------------

Create a workflow ``workflow.py``:

.. code-block:: python

   import imas
   from my_first_actor import MyFirstActor
   
   # Create IDS objects
   _factory = imas.IDSFactory()
   equilibrium = _factory.equilibrium()
   core_profiles = _factory.core_profiles()
   
   # Initialize and run actor
   actor = MyFirstActor()
   actor.init()
   actor.run(equilibrium, core_profiles)
   actor.finalize()

Run it:

.. code-block:: bash

   python workflow.py

Your First MUSCLE3 Actor
========================================

Prerequisites
-------------

Ensure MUSCLE3 is installed:

.. code-block:: bash

   pip install iwrap[muscle3]
   # Verify
   python -c "import muscle3; print(muscle3.__version__)"

Step 1: Prepare Your Code
--------------------------

Create a MUSCLE3-compatible code ``my_muscle3_code.py``:

.. code-block:: python

   # my_muscle3_code.py
   def init():
       """Initialize - no IDS arguments allowed"""
       print("MUSCLE3 actor initializing...")
   
   def run(equilibrium_in, core_profiles_out):
       """Main computation with IDS"""
       print("MUSCLE3 step running...")
       # Your physics here
       return equilibrium_in, core_profiles_out
   
   def finalize():
       """Cleanup - no IDS arguments allowed"""
       print("MUSCLE3 actor finalizing...")

Step 2: Create Code Description
--------------------------------

Create ``muscle3_code_description.yaml``:

.. code-block:: yaml

   actor_description:
     actor_name: my_muscle3_actor
     actor_type: MUSCLE3-Python
   
   code_description:
     implementation:
       programming_language: python
       code_path: ./my_muscle3_code.py
       
       subroutines:
         init:
           name: init
           arguments: []  # IMPORTANT: No IDS for MUSCLE3 init
         
         step:
           name: run
           arguments:
             - name: equilibrium
               type: input
             - name: core_profiles
               type: output
         
         finalize:
           name: finalize
           arguments: []  # IMPORTANT: No IDS for MUSCLE3 finalize

Step 3: Generate MUSCLE3 Actor
-------------------------------

.. code-block:: bash

   iwrap -a my_muscle3_actor -t MUSCLE3-Python -f muscle3_code_description.yaml

Step 4: Create MUSCLE3 Workflow
--------------------------------

Create ``workflow.ymmsl``:

.. code-block:: yaml

   ymmsl_version: v0.1
   
   model:
     name: my_first_muscle3_simulation
     components:
       macro:
         implementation: macro_model
         ports:
           o_i: core_profiles
           s_o: equilibrium
       
       micro:
         implementation: my_muscle3_actor
         ports:
           f_init: equilibrium
           o_f: core_profiles
   
   settings:
     micro.time_step: 0.1
     macro.iterations: 10

Step 5: Run MUSCLE3 Workflow
-----------------------------

.. code-block:: bash

   muscle_manager workflow.ymmsl

Using the GUI
========================================

iWrap provides a graphical interface for easy actor generation.

Launch GUI
----------

.. code-block:: bash

   iwrap-gui

Or load an existing description:

.. code-block:: bash

   iwrap-gui -f code_description.yaml

GUI Steps
---------

1. **Select Actor Type:** Choose from dropdown (python, MUSCLE3-Python, etc.)
2. **Enter Actor Name:** Specify your actor name
3. **Code Details:** Browse and select your code file
4. **Define Methods:** Add init, step, finalize methods
5. **Specify IDS Arguments:** Define input/output IDS for each method
6. **Generate:** Click generate button

The GUI will create the YAML file and generate the actor.

Common Tasks
========================================

Listing Available Actor Types
------------------------------

.. code-block:: bash

   iwrap --list-actor-types

Getting Help
------------

.. code-block:: bash

   iwrap -h
   iwrap-gui -h

Checking Version
----------------

.. code-block:: bash

   iwrap --version

Viewing Actor Generator Details
--------------------------------

.. code-block:: bash

   iwrap --list-actor-details python
   iwrap --list-actor-details MUSCLE3-Python

Common Issues and Solutions
========================================

Issue: MUSCLE3 Generators Not Available
----------------------------------------

**Symptom:** Only ``python`` appears in ``--list-actor-types``

**Solution:**

.. code-block:: bash

   pip install iwrap[muscle3]

Issue: Import Error
-------------------

**Symptom:** ``ModuleNotFoundError: No module named 'iwrap'``

**Solution:** Ensure iWrap is installed and PYTHONPATH is set:

.. code-block:: bash

   pip install iwrap
   # Or use environment setup script
   source set-iter.sh

Issue: MUSCLE3 Init/Finalize Error
-----------------------------------

**Symptom:** ``ValueError: MUSCLE3 actor generator cannot handle INIT/FINALIZE methods with IDS arguments``

**Solution:** Remove IDS arguments from init and finalize methods in YAML:

.. code-block:: yaml

   subroutines:
     init:
       name: init
       arguments: []  # Empty - no IDS allowed
     finalize:
       name: finalize
       arguments: []  # Empty - no IDS allowed

Next Steps
========================================

Now that you've created your first actor, explore:

📚 **Documentation**
  - :doc:`actor_types` - Learn about all actor types
  - :doc:`muscle3_actors` - Deep dive into MUSCLE3
  - :doc:`project_description` - Complete actor description reference

🎓 **Tutorials**
  - Follow interactive Jupyter tutorials in ``docs/tutorial/``
  - Try example actors in ``examples/``

🔧 **Advanced Topics**
  - :doc:`code_standardization` - Code requirements and best practices
  - :doc:`developers_manual/index` - Extending iWrap
  - :doc:`actor_usage` - Using generated actors in workflows

💡 **Examples**
  - Browse ``examples/`` directory for complete working examples
  - Check plugin tests for advanced usage patterns

Getting Help
========================================

If you need assistance:

1. Check the documentation: :doc:`../index`
2. Review examples in the repository
3. Contact: iWrap Development Team

Happy coding with iWrap! 🚀
