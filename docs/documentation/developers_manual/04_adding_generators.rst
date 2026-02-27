#######################################################################################################################
Adding New Generators
#######################################################################################################################

The existing set of generators can be easily extended in one of two ways:

* As a **built-in** generator, by adding the implemented class(es) as an integral part of iWrap to the iWrap repository.
* As a **plug-in**, using Python "namespace packages" (see `here <https://packaging.python.org/guides/packaging-namespace-packages/>`_
  for a detailed description of this concept).

Regardless of the chosen approach, the implementation of the generator remains the same. The only difference lies in 
where it is placed.

===================== ========================================= ============================= ==================
   Generator Type               Built-in Package                       Plug-in Package         Interface
===================== ========================================= ============================= ==================
  Actor Generator      ``iwrap.generators.actor_generators``     ``iwrap_actor_generator``     ActorGenerator
--------------------- ----------------------------------------- ----------------------------- ------------------
  Binder Generator     ``iwrap.generators.binder_generators``    ``iwrap_binder_generator``    BinderGenerator
--------------------- ----------------------------------------- ----------------------------- ------------------
  Wrapper Generator    ``iwrap.generators.wrapper_generators``   ``iwrap_wrapper_generator``   WrapperGenerator
===================== ========================================= ============================= ==================

* ``Generator Type`` - Type of the implemented generator.
* ``Built-in Package`` - The location in the iWrap repository structure where a "built-in" generator should be placed.
* ``Plug-in Package`` - The name of the package where a "plugged-in" generator should be placed.
* ``Interface`` - The interface (abstract class) that the generator must implement.

Implementation of the Generator
#######################################################################################################################

A generator for a particular actor layer must inherit from the appropriate base class. Depending on the layer 
being generated, it should implement the abstract methods of one of the following base classes:

* ``ActorGenerator`` class (defined in package :py:mod:`iwrap.generators.actor_generators`)
* ``BinderGenerator`` class (defined in package :py:mod:`iwrap.generators.binder_generators`)
* ``WrapperGenerator`` class (defined in package :py:mod:`iwrap.generators.wrapper_generators`)

Making the New Generator Discoverable
#######################################################################################################################

The added generator will be found by iWrap if and only if:

* It implements the correct interface (abstract class).
* It is placed in the proper repository location (for a built-in generator).
* It is placed in a correctly named package (for a plugged-in generator).
* The directory containing the package with the plugged-in generator is added to ``PYTHONPATH``.

.. hint::
   Using the CLI to list all actor types (``iwrap --list-actor-types``), developers can easily check if a new actor type 
   generator was found and properly loaded by iWrap.

Plugins <--> iWrap API Compatibility
#######################################################################################################################

See :doc:`05_plugins_compatibility` for a full description of the iWrap plugin API versioning and
compatibility mechanism.

