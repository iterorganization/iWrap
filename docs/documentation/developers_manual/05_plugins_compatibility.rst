#######################################################################################################################
Plugins <--> iWrap API Compatibility
#######################################################################################################################

Plugins, thanks to their modularity, allow users to compose iWrap from only the components necessary for a given purpose, 
omitting unused functionalities. However, as both iWrap and the plugins continue to evolve, ensuring cross-compatibility 
between specific versions of iWrap and the plugins can be challenging. To address potential incompatibilities, a compatibility 
check mechanism has been proposed.

iWrap API Versioning
#######################################################################################################################

iWrap maintains the current version of the plugin API using the ``iwrap.generators.API_VERSION`` attribute.

  .. note::
       iWrap can be queried from the command line to check the current version of the API used
       for communication between iWrap and plugins:

       .. code-block:: bash

           bash> iwrap --plugins-api-version
           iWrap <-> plugins API version:   2.0

Plugin API Versioning
#######################################################################################################################

Every plugin should declare the API version with which it is compliant by using the class attribute ``COMPLIANT_API``.

.. code-block:: python

   class AbstractGenerator(ABC):

        COMPLIANT_API: str = 'Major.minor'

        @classmethod
        def check_api_compliance(cls) -> None:
            ...

Compatibility Check
#######################################################################################################################

* The class method ``check_api_compliance()`` determines whether the plugin is compatible with the current version 
  of iWrap or if it may be incompatible.
* The method checks the API version currently handled by iWrap (``Mi.mi``) against the compliant version declared 
  by the plugin (``Mp.mp``).
* The method detects **INCOMPATIBILITY** if:

  + The plugin does not implement versioning API.
  + The major versions differ (``Mp != Mi``).
  + The plugin API version is newer than the iWrap API version (``Mp.mp > Mi.mi``), meaning the plugin may use changes 
    that are not yet available in the current iWrap version.

* **COMPATIBILITY** is assumed only if:

  + The major versions are equal (``Mp == Mi``), and
  + The plugin API version is older or equal to the iWrap API version (``Mp.mp <= Mi.mi``).

* If the plugin is not compatible, the user is informed of the error, and the plugin is not loaded.

.. hint::
  The ``check_api_compliance()`` method can be overridden in generator implementation classes to better address 
  compatibility issues specific to the particular plugin.

