========================================
Actor Types Overview
========================================

iWrap supports multiple actor generator types, each designed for specific use cases and integration scenarios.

Available Actor Types
========================================

Built-in Generators
-------------------

iWrap includes the following built-in actor generators:

Standard Python Actor
~~~~~~~~~~~~~~~~~~~~~

**Type ID:** ``python``

**Description:** Standard Python actors for straightforward Python integrations with IMAS workflows.

**Use Cases:**
- Simple Python-based physics codes
- Prototyping and development
- Direct IMAS IDS data access
- Python-to-Python workflows

**Installation:** Included in core iWrap (no extra dependencies)

**Documentation:** :doc:`project_description`

MUSCLE3 Actors (Optional)
~~~~~~~~~~~~~~~~~~~~~~~~~~

MUSCLE3 (Multiscale Coupling Library and Environment) actors enable high-performance multiscale and multiphysics coupling scenarios.

**Installation Required:** ``pip install iwrap[muscle3]``

Available MUSCLE3 Generators:

**MUSCLE3-Python**
  :Type ID: ``MUSCLE3-Python``
  :Language: Python
  :Use Case: Python physics codes in MUSCLE3 workflows
  :Documentation: :doc:`muscle3_actors`

**MUSCLE3-CPP**
  :Type ID: ``MUSCLE3-CPP``
  :Language: C++
  :Use Case: High-performance C++ codes with MUSCLE3 coupling
  :Documentation: :doc:`muscle3_actors`

**MUSCLE3-Fortran**
  :Type ID: ``MUSCLE3-Fortran``
  :Language: Fortran
  :Use Case: Fortran codes or legacy codes with MUSCLE3 integration
  :Documentation: :doc:`muscle3_actors`

Choosing an Actor Type
========================================

Decision Matrix
---------------

Use this matrix to help choose the appropriate actor type:

+----------------------------+------------------+------------------------+
| Requirement                | Standard Python  | MUSCLE3 Actors         |
+============================+==================+========================+
| Simple Python workflow     | ✅ Best choice   | ❌ Overkill            |
+----------------------------+------------------+------------------------+
| Multiscale coupling        | ❌ Manual        | ✅ Built-in            |
+----------------------------+------------------+------------------------+
| Multi-language workflow    | ✅ Supported     | ✅ Optimized           |
+----------------------------+------------------+------------------------+
| High-performance coupling  | ⚠️ Manual setup  | ✅ Built-in            |
+----------------------------+------------------+------------------------+
| Complex time scales        | ❌ Manual        | ✅ Built-in            |
+----------------------------+------------------+------------------------+
| Learning curve             | ✅ Low           | ⚠️ Medium              |
+----------------------------+------------------+------------------------+
| External dependencies      | ✅ None          | ⚠️ MUSCLE3 required    |
+----------------------------+------------------+------------------------+

Recommendation Guidelines
--------------------------

**Choose Standard Python Actor if:**
- You have a simple Python-based physics code
- You're building a straightforward sequential workflow
- You don't need complex multiscale coupling
- You want minimal dependencies

**Choose MUSCLE3 Actor if:**
- You need multiscale or multiphysics coupling
- Your workflow involves multiple time scales
- You need high-performance parallel coupling
- You're building complex coupled simulations
- You need standardized coupling interfaces

Listing Available Actor Types
========================================

To see which actor types are available in your iWrap installation:

.. code-block:: bash

   iwrap --list-actor-types

Example output (core only):

.. code-block:: text

            Id          :              Name              : Description
   ----------------------------------------------------------------------
          python        :             python             : python

Example output (with MUSCLE3):

.. code-block:: text

            Id          :              Name              : Description
   ----------------------------------------------------------------------
       MUSCLE3-CPP      :         MUSCLE3 (C++)          : Wrapping C++ code into MUSCLE3 micro model
     MUSCLE3-Fortran    :       MUSCLE3 (Fortran)        : Wrapping Fortran code into MUSCLE3 micro model
      MUSCLE3-Python    :        MUSCLE3 (Python)        : Wrapping Python code into MUSCLE3 micro model
          python        :             python             : python

Getting Details About an Actor Type
====================================

Get detailed information about a specific actor type:

.. code-block:: bash

   iwrap --list-actor-details python
   iwrap --list-actor-details MUSCLE3-Python

This will show:
- Supported code languages
- Supported data types
- API version
- Additional requirements

Comparison Table
========================================

Feature Comparison
------------------

+---------------------------+-------------------+-------------------+-------------------+-------------------+
| Feature                   | Python Actor      | MUSCLE3-Python    | MUSCLE3-CPP       | MUSCLE3-Fortran   |
+===========================+===================+===================+===================+===================+
| **Actor Language**        | Python            | Python            | Python            | Python            |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Code Language**         | Python,           | Python            | C++               | Fortran           |
|                           | Fortran, C++, Java|                   |                   |                   |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Coupling Framework**    | Direct calls      | MUSCLE3           | MUSCLE3           | MUSCLE3           |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Multiscale Support**    | Manual            | Built-in          | Built-in          | Built-in          |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Init/Finalize IDS**     | ✅ Allowed        | ❌ Not allowed    | ❌ Not allowed    | ❌ Not allowed    |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Workflow Description**  | Python script     | yMMSL             | yMMSL             | yMMSL             |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Extra Dependencies**    | None              | MUSCLE3           | MUSCLE3           | MUSCLE3           |
+---------------------------+-------------------+-------------------+-------------------+-------------------+
| **Performance**           | Good              | Excellent         | Excellent         | Excellent         |
+---------------------------+-------------------+-------------------+-------------------+-------------------+

Usage Examples
========================================

Generating Different Actor Types
---------------------------------

**Standard Python Actor:**

.. code-block:: bash

   iwrap -a my_actor -t python -f code_description.yaml

**MUSCLE3 Python Actor:**

.. code-block:: bash

   iwrap -a my_actor -t MUSCLE3-Python -f code_description.yaml

**MUSCLE3 C++ Actor:**

.. code-block:: bash

   iwrap -a my_actor -t MUSCLE3-CPP -f code_description.yaml

**MUSCLE3 Fortran Actor:**

.. code-block:: bash

   iwrap -a my_actor -t MUSCLE3-Fortran -f code_description.yaml

External Plugin Support
========================================

iWrap's plugin architecture allows for external actor generators to be developed and distributed separately.

How External Plugins Work
--------------------------

1. External plugins are Python packages with the namespace ``iwrap_actor_generator``
2. They are automatically discovered when installed
3. They appear alongside built-in generators

Creating External Plugins
--------------------------

See :doc:`developers_manual/04_adding_generators` for details on creating custom actor generators.

Installing External Plugins
----------------------------

External plugins can be installed via pip:

.. code-block:: bash

   pip install your-custom-iwrap-plugin

After installation, they will appear in the list of available actor types.

See Also
========================================

- :doc:`muscle3_actors` - MUSCLE3 actor generators documentation
- :doc:`project_description` - Creating actor descriptions
- :doc:`actor_generation_cmdln` - Command-line usage
- :doc:`iwrap_gui` - Graphical interface
- :doc:`developers_manual/04_adding_generators` - Creating custom generators
