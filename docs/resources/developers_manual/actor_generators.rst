#######################################################################################################################
Actor generators
#######################################################################################################################



.. toctree::

Concept of 'generators'
#######################################################################################################################

To create an actor iWrap uses set of ``generators``. Every ``generator`` is responsible for generation of
only one of actor layers (see :ref:`actor layers description <actor_layers_anchor>` for details):

* *Actor layer* or
* *Binding layer* or
* *Wrapping layer*

Moreover, due to variety of actors wrapping various languages, handling different types of IDS etc, etc,
iWrap keeps and manages lists of generators for every kind of layer, specialized for handling given language, data type
or just providing different logic.

iWraps, during generation process, picks up one ``generator`` per layer, and then sequentially fires its methods,
to: generate, build and install given component (layer)


Generator API
#######################################################################################################################

.. code-block:: python

   class AbstractGenerator( ABC ):

    COMPLIANT_API: str = 'Major.minor'

    @classmethod
    def check_api_compliance(cls,) -> None:
        ...

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    @abstractmethod
    def initialize(self, actor_settings: dict):
        ...

    @abstractmethod
    def generate(self, actor_settings: dict):
        ...

    @abstractmethod
    def build(self, actor_settings: dict):
        ...

    @abstractmethod
    def install(self, actor_settings: dict):
        ...

    @abstractmethod
    def cleanup(self, actor_settings: dict):
        ...


Every generator must be derived from ``AbstractGenerator`` class and implement following methods:

* ``initialize`` - initializes generator environment
* ``generate`` - creates all layer files
* ``build`` - builds the code (if e.g. the code require compilation)
* ``install`` - installs components of given layers, e.g. moving them from temporary build directory to the final one
* ``cleanup`` - performs clean up operations, removing temporary files, directories, releasing resources etc.

.. note::
   Because there are lots of possible combinations of various components being involved in the processing,
   there is no single, correct, way of implementing a generator. It fully depends on the actor desired logic,
   programming languages used for processing, data types being passed between components, etc.

**Generator metadata**

Besides ``AbstractGenerator`` methods, generator classes must provide additional metadata that describe given generator
and allow iWrap to chose a proper one for generation.
These metadata describes:


* ``name`` - user defined name of generator
* ``description`` - user friendly description of generator
* ``actor_language`` - programming language of the actor highest layers (and workflow)
* ``actor_data_types`` - a list of types of data (IDSes) handled by the actor highest layers (and workflow)
* ``code_languages`` - a list of code programming languages that are handled by binder and wrapper
* ``code_data_types`` - a list of types of data (IDSes) handled by binder and wrapper



**Actor generators**

.. code-block:: python

 class ActorGenerator(AbstractGenerator):

    @property
    @abstractmethod
    def name(self) -> str:
        ...

    @property
    @abstractmethod
    def description(self) -> str:
        ...

    @property
    @abstractmethod
    def actor_language(self) -> str:
        ...

    @property
    @abstractmethod
    def actor_data_types(self) -> Set[str]:
        ...


**Binder generators**

.. code-block:: python

 class BinderGenerator(AbstractGenerator):

    @property
    @abstractmethod
    def name(self) -> str:
        ...

    @property
    @abstractmethod
    def description(self) -> str:
        ...

    @property
    @abstractmethod
    def actor_language(self) -> str:
        ...

    @property
    @abstractmethod
    def actor_data_types(self) -> Set[str]:
        ...

    @property
    @abstractmethod
    def code_data_types(self) -> Set[str]:
        ...

    @property
    @abstractmethod
    def code_languages(self) -> Set[str]:
        ...

**Wrapper generators**

.. code-block:: python

 class WrapperGenerator(AbstractGenerator):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __str__(self):
        return self.name

    @property
    @abstractmethod
    def name(self) -> str:
        ...

    @property
    @abstractmethod
    def description(self) -> str:
        ...

    @property
    @abstractmethod
    def code_data_types(self) -> Set[str]:
        ...

    @property
    @abstractmethod
    def code_language(self) -> str:
        ...

Generated actor configuration
#######################################################################################################################

Description of WHAT, HOW and WHERE build an actor is passed to generators' methods as
``actor_settings`` Python dictionary. This data structure gathers three entries ('subdictionaries') at its root:

* ``actor_description`` - describes the actor parameters (see :ref:`here <yaml_actor_description_anchor>` for details)

* ``code_description``  - describes the code to be wrapped (see :ref:`here <yaml_code_description_anchor>` for details)

* ``platform_settings`` - describe the values default for given platform/installation
  (see :ref:`here <yaml_platform_settings_anchor>` for details)


Data of ``actor_settings`` can be accessed as an ordinary Python dictionary. It is up to the developer,
which entries of provided data will be used and how they will be used.

Adding new generators
#######################################################################################################################
The existing set of generators can be easily extended. It can be done in one of two ways:

* as a 'built-in' generator, adding implemented class(es) as the integral part of iWrap to the iWrap repository
* as a 'plug-in', using  Python 'namespace packages' (see `here <https://packaging.python.org/guides/packaging-namespace-packages/>`_
  for a detailed description of this concept).

Independently on the chosen way, an implementation of generator is exactly the same. The only difference is
a place where it is put.

===================== ======================================= ========================= ==========
   generator type               built-in package                 plug-in package         interface
===================== ======================================= ========================= ==========
  actor generator      'iwrap.generators.actor_generators'     'iwrap_actor_generator'  ActorGenerator
--------------------- --------------------------------------- ------------------------- ----------
  binder generator     'iwrap.generators.binder_generators'    iwrap_binder_generator'  BinderGenerator
--------------------- --------------------------------------- ------------------------- ----------
  wrapper generator    'iwrap.generators.wrapper_generators'  'iwrap_wrapper_generator' WrapperGenerator
===================== ======================================= ========================= ==========

* generated type - type of implemented generator
* built-in package - a place in iWrap repository structure, where 'built-in' generator should be put
* plug-in package - a name of a package,  where 'plugged-in' generator should be put
* interface - an interface (an abstract class) to be implemented by generator

.. warning::
   Added generator will be found by iWrap, if and only if:

   * It implements a proper interface (abstract class)
   * It is put in a proper repository location (built-in generator)
   * It is put in a correctly named package (plugged-in generator)
   * The directory containing package with plugged-in generator is added to ``PYTHONPATH``

Plugins <--> iWrap API compatibility
#######################################################################################################################
Plugins, thanks to their modularity, allow users to compose iWrap only from components that are necessary
for a given purpose, omitting unused functionalities. However, as both iWrap and the plug-ins continue to evolve,
ensuring cross-compatibility between a specific version of iWrap and the plug-ins is a challenge.
To warn user about the potential incompatibilities, following mechanism has been proposed:


* iWrap keeps the current version of the plugin API (`iwrap.generators.API_VERSION`)

  .. note::
       iWrap can be queried from the commandline to check the current version of the API used
       to communicate between iWrap and plugins

       .. code-block:: bash

           bash> iwrap --plugins-api-version
           iWrap <-> plugins API version:   2.0


* Every plugin should declare the compliant API using class attribute `COMPLIANT_API`
* The class method `check_api_compliance()` determines whether the plugin is definitely compatible with the current version
  of the iWrap or if it could be not compatible.
* The method checks the API version currently handled by iWrap (Mi.mi) against declared
  compliant version handled by the plugin (Mp.mp)
* The method detects INCOMPATIBILITY if:

  + The plugin doesn't implement versioning API
  + The major versions differs (Mp != Mi)
  + The plugin API version is newer than iWrap one (Mp.mp > Mi.mi), i.e.: plugin may use changes
    not yet available in the current

* The COMPATIBILITY is assumed only if:
   + The major versions are equal (Mp == Mi) and
   + The plugin API version is older or equal than iWrap one (Mp.mp <= Mi.mi)

* If the plugin is not compatible, the user is informed about the error and the plugin is not loaded.

  This method can be overwritten in generator implementation classes to better address issues
  related with compatibility of the particular plugin

.. code-block:: python

   class AbstractGenerator( ABC ):

        COMPLIANT_API: str = 'Major.minor'

        @classmethod
        def check_api_compliance(cls) -> None:
            ...




.. Example-TBA
.. #######################################################################################################################

