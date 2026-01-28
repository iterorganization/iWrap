========================================
Migration Guide: MUSCLE3 Plugin Integration
========================================

This guide helps users and developers migrate from the external ``iwrap-plugins-muscle3`` package to the new built-in MUSCLE3 generators in iWrap core.

What Changed?
========================================

MUSCLE3 Actor Generators are Now Built-in
------------------------------------------

Previously, MUSCLE3 support was provided through a separate ``iwrap-plugins-muscle3`` package. 
Now, MUSCLE3 generators are **built directly into iWrap core**, making installation and usage simpler.

**Before:**

.. code-block:: bash

   # Old installation (two packages)
   pip install iwrap
   pip install iwrap-plugins-muscle3

**After:**

.. code-block:: bash

   # New installation (single package with optional feature)
   pip install iwrap[muscle3]

Benefits
--------

✅ **Simpler Installation:** One command instead of two packages

✅ **Unified Version Management:** Core and MUSCLE3 generators always compatible

✅ **Better Integration:** MUSCLE3 generators maintained alongside core

✅ **Easier Updates:** Update both core and MUSCLE3 features together

Migration Steps
========================================

For End Users
-------------

**Step 1: Uninstall Old Plugin**

.. code-block:: bash

   pip uninstall iwrap-plugins-muscle3

**Step 2: Update iWrap**

.. code-block:: bash

   pip install --upgrade iwrap[muscle3]

**Step 3: Verify**

.. code-block:: bash

   iwrap --list-actor-types

You should see all MUSCLE3 generators listed.

**Step 4: Test Your Workflows**

Your existing YAML files and workflows should work without changes:

.. code-block:: bash

   # This should work exactly as before
   iwrap -a my_actor -t MUSCLE3-Python -f my_code.yaml

For Developers
--------------

**Step 1: Update Development Environment**

.. code-block:: bash

   # Remove old plugin
   pip uninstall iwrap-plugins-muscle3
   
   # Install new version in development mode
   cd /path/to/iwrap
   pip install -e .[all]

**Step 2: Update Import Statements (if you extended generators)**

If you created custom generators based on MUSCLE3, update imports:

**Before:**

.. code-block:: python

   from iwrap_plugins.iwrap_actor_generator.muscle3_common import m3_utils

**After:**

.. code-block:: python

   from iwrap.generators.actor_generators.muscle3_common import m3_utils

**Step 3: Update Tests**

Test discovery may need updates if you had custom tests:

.. code-block:: python

   # Old test path
   from iwrap_plugins.iwrap_actor_generator.muscle3_python import PythonActorGenerator
   
   # New test path
   from iwrap.generators.actor_generators.muscle3_python.m3_python_actor import PythonActorGenerator

For CI/CD Pipelines
-------------------

**Update CI Configuration**

**Before (.gitlab-ci.yml / .github/workflows):**

.. code-block:: yaml

   install:
     script:
       - pip install iwrap
       - pip install iwrap-plugins-muscle3

**After:**

.. code-block:: yaml

   install:
     script:
       - pip install iwrap[muscle3]

**Update Test Matrix**

Consider testing with and without MUSCLE3:

.. code-block:: yaml

   test-core:
     script:
       - pip install iwrap  # Core only
       - pytest -m "not muscle3"
   
   test-with-muscle3:
     script:
       - pip install iwrap[muscle3]
       - pytest  # All tests including MUSCLE3

Compatibility
========================================

Backward Compatibility
----------------------

✅ **YAML Files:** All existing code description YAML files work without changes

✅ **Actor Types:** Same actor type IDs (``MUSCLE3-Python``, ``MUSCLE3-CPP``, ``MUSCLE3-Fortran``)

✅ **Command Line:** Same CLI arguments and options

✅ **Workflows:** Existing yMMSL workflow files work without changes

✅ **Generated Actors:** Same actor structure and behavior

API Version
-----------

MUSCLE3 generators updated from API 2.0 to API 2.1 to match iWrap core.

This is an internal change and should not affect end users.

Breaking Changes
----------------

⚠️ **None for end users**

⚠️ **For developers:** Import paths changed (see above)

Troubleshooting
========================================

Issue: Both Old and New Installed
----------------------------------

**Symptom:** Conflicts or duplicate generators listed

**Solution:**

.. code-block:: bash

   # Remove both
   pip uninstall iwrap iwrap-plugins-muscle3
   
   # Reinstall fresh
   pip install iwrap[muscle3]

Issue: MUSCLE3 Generators Not Found
------------------------------------

**Symptom:** Only ``python`` generator listed, no MUSCLE3

**Check:**

.. code-block:: bash

   # Verify MUSCLE3 extra was installed
   pip show iwrap
   
   # Look for: Requires: muscle3>=0.7.0

**Solution:**

.. code-block:: bash

   pip install muscle3>=0.7.0
   # OR
   pip install --upgrade iwrap[muscle3]

Issue: Import Errors in Custom Code
------------------------------------

**Symptom:** ``ModuleNotFoundError: No module named 'iwrap_plugins'``

**Cause:** Custom code still using old import paths

**Solution:** Update imports as shown in "For Developers" section

Frequently Asked Questions
========================================

Can I still use the old plugin?
--------------------------------

No. The ``iwrap-plugins-muscle3`` package is deprecated and will not receive updates. 
Please migrate to the built-in MUSCLE3 generators.

Do I need to change my YAML files?
-----------------------------------

No. All existing code description YAML files work without any changes.

Will my old workflows break?
-----------------------------

No. Your yMMSL workflow files and Python scripts work without changes.

What if I don't need MUSCLE3?
------------------------------

Simply install iWrap without the ``[muscle3]`` extra:

.. code-block:: bash

   pip install iwrap

MUSCLE3 generators won't be available, but core functionality works perfectly.

Can I install MUSCLE3 support later?
-------------------------------------

Yes:

.. code-block:: bash

   # Install core first
   pip install iwrap
   
   # Add MUSCLE3 support later
   pip install muscle3>=0.7.0

What about future plugins?
---------------------------

iWrap still supports external plugins! The plugin discovery mechanism is unchanged.
Future plugin developers can still create ``iwrap_actor_generator`` namespace packages.

Timeline and Support
========================================

Release Timeline
----------------

- **iWrap v0.7.x and earlier:** External ``iwrap-plugins-muscle3`` required
- **iWrap v0.8.0+:** MUSCLE3 built-in, plugin deprecated
- **Future:** Plugin package will be archived (read-only)

Support Policy
--------------

- **Built-in MUSCLE3:** Fully supported, actively maintained
- **External plugin:** Deprecated, no new features, critical bugs only
- **After 6 months:** External plugin unsupported

Getting Help
========================================

If you encounter issues during migration:

1. Check this migration guide
2. Review the :doc:`muscle3_actors` documentation
3. Check existing issues on the iWrap repository
4. Contact: iWrap Development Team

Additional Resources
========================================

- :doc:`muscle3_actors` - Complete MUSCLE3 documentation
- :doc:`installation_guide/iwrap_installation` - Installation instructions
- :doc:`developers_manual/04_adding_generators` - Creating custom generators
- `MUSCLE3 Documentation <https://muscle3.readthedocs.io/>`_
