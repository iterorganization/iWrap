#######################################################################################################################
Adding New Generators
#######################################################################################################################

The existing set of generators can be easily extended in one of two ways:

* As a **built-in** generator, by adding the implemented class(es) as an integral part of iWrap to the iWrap repository.
* As a **plug-in**, using Python "namespace packages" (see `here <https://packaging.python.org/guides/packaging-namespace-packages/>`_
  for a detailed description of this concept).

Regardless of the chosen approach, the implementation of the generator remains the same. The only difference lies in 
where it is placed.

===================== ======================================= ========================= ==========
   Generator Type               Built-in Package                 Plug-in Package         Interface
===================== ======================================= ========================= ==========
  Actor Generator      ``iwrap.generators.actor_generators``     ``iwrap_actor_generator``  ActorGenerator
--------------------- --------------------------------------- ------------------------- ----------
  Binder Generator     ``iwrap.generators.binder_generators``    ``iwrap_binder_generator``  BinderGenerator
--------------------- --------------------------------------- ------------------------- ----------
  Wrapper Generator    ``iwrap.generators.wrapper_generators``  ``iwrap_wrapper_generator`` WrapperGenerator
===================== ======================================= ========================= ==========

* ``Generator Type`` - Type of the implemented generator.
* ``Built-in Package`` - The location in the iWrap repository structure where a "built-in" generator should be placed.
* ``Plug-in Package`` - The name of the package where a "plugged-in" generator should be placed.
* ``Interface`` - The interface (abstract class) that the generator must implement.

Implementation of the Generator
#######################################################################################################################

A generator for a particular actor layer must inherit from the appropriate base class. Depending on the layer 
being generated, it should implement the abstract methods of one of the following base classes:

* ``ActorGenerator`` class (defined in package `iwrap.generators.actor_generators <https://git.iter.org/projects/IMEX/repos/iwrap/browse/iwrap/generators/actor_generators/__init__.py>`_)
* ``BinderGenerator`` class (defined in package `iwrap.generators.binder_generators <https://git.iter.org/projects/IMEX/repos/iwrap/browse/iwrap/generators/binder_generators/__init__.py>`_)
* ``WrapperGenerator`` class (defined in package `iwrap.generators.wrapper_generators <https://git.iter.org/projects/IMEX/repos/iwrap/browse/iwrap/generators/wrapper_generators/__init__.py>`_)

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

Plugins, thanks to their modularity, allow users to compose iWrap from only the components necessary for a given purpose, 
omitting unused functionalities. However, as both iWrap and the plug-ins evolve, ensuring cross-compatibility between a 
specific version of iWrap and the plug-ins can be challenging. To warn users about potential incompatibilities, the following 
mechanism has been proposed:

* iWrap maintains the current version of the plugin API (``iwrap.generators.API_VERSION``).

  .. note::
       iWrap can be queried from the command line to check the current version of the API used
       to communicate between iWrap and plugins.

       .. code-block:: bash

           bash> iwrap --plugins-api-version
           iWrap <-> plugins API version:   2.0

* Every plugin should declare the compliant API using the class attribute ``COMPLIANT_API``.
* The class method ``check_api_compliance()`` determines whether the plugin is definitely compatible with the current version 
  of iWrap or if it may not be compatible.
* The method checks the API version currently handled by iWrap (``Mi.mi``) against the declared compliant version handled 
  by the plugin (``Mp.mp``).
* The method detects INCOMPATIBILITY if:

  + The plugin doesn't implement versioning API.
  + The major versions differ (``Mp != Mi``).
  + The plugin API version is newer than iWrap's (``Mp.mp > Mi.mi``), i.e., the plugin may use changes
    not yet available in the current version of iWrap.

* COMPATIBILITY is assumed only if:
   + The major versions are equal (``Mp == Mi``) and
   + The plugin API version is older or equal to iWrap's (``Mp.mp <= Mi.mi``).

* If the plugin is not compatible, the user is informed of the error, and the plugin is not loaded.

  This method can be overridden in generator implementation classes to better address issues
  related to the compatibility of the particular plugin.

.. code-block:: python

   class AbstractGenerator(ABC):

        COMPLIANT_API: str = 'Major.minor'

        @classmethod
        def check_api_compliance(cls) -> None:
            ...

