========================================
Quick Start Guide
========================================

This guide will help you get started with iWrap quickly, covering both standard Python actors and MUSCLE3 actors.

Installation
============

For detailed installation instructions, see :doc:`installation_guide/iwrap_installation`.

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
For detailed installation instructions, see :doc:`installation_guide/iwrap_installation`.

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
