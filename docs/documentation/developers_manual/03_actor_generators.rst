#######################################################################################################################
Actor generators
#######################################################################################################################


Concept of 'generators'
#######################################################################################################################

In iWrap, actors are created using a set of components called generators. Each generator is responsible for the generation of only one specific layer of an actor (as described in the previous chapter):

* *Actor layer* or
* *Binding layer* or
* *Wrapping layer*

Developers have considerable flexibility in the design and implementation of each layer. However, ``The Actor API Layer`` is mandatory and must be implemented. ``The Binding Layer`` and ``The Wrapping Layer`` are usually implemented only when necessary to ensure proper data transmission and subroutine calls between different programming languages.

iWrap maintains and manages lists of generators for each type of layer, specialized for handling specific languages, data types, or providing unique logic. During the generation process, the iWrap engine selects one ``generator`` per layer and sequentially executes its methods to generate, build, and install the corresponding layer.

.. note::
   Due to the wide range of components and configurations possible in the generation process, there is no single "correct" way to implement a generator. The implementation fully depends on the desired actor logic, the programming languages in use, the data types being passed between components, and other specific requirements.


Generator API
#######################################################################################################################


Generator methods
==================

Every generator must inherit from the ``AbstractGenerator`` class and implement the following methods:

.. code-block:: python

   class AbstractGenerator( ABC ):

    COMPLIANT_API: str = 'Major.minor'

    @classmethod
    def check_api_compliance(cls,) -> None:
        ...

    def configure(self, info_output_stream=sys.stdout):
        ...

    @abstractmethod
    def initialize(self, actor_settings: dict):
        ...

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


The required methods for each generator are as follows:

* ``initialize`` - Initializes the generator environment.
* ``validate`` - Ensures that the generator has received all the necessary information required to generate a layer.
* ``generate`` - Creates all the code for the generated layer.
* ``build`` - Builds the code, such as compiling if necessary.
* ``install`` - Installs components of the generated layer, typically by moving them from a temporary build directory to their final destination.
* ``cleanup`` - Performs cleanup operations, such as removing temporary files and directories, and releasing resources.


Generator description
======================

In addition to implementing the methods of the ``AbstractGenerator`` class, generator classes must provide additional metadata that describe the generator and allow iWrap to select the appropriate one for the generation process. This metadata includes:

* ``type`` - The type of the generator.
* ``name`` - The user-defined name of the generator.
* ``description`` - A user-friendly description of the generator.
* ``actor_language`` - The programming language of the actor's highest layers (and workflow).
* ``actor_data_types`` - A list of data types (e.g., IDSes) handled by the actor's highest layers (and workflow).
* ``code_languages`` - A list of programming languages handled by the binder and wrapper.
* ``code_data_types`` - A list of data types (e.g., IDSes) handled by the binder and wrapper.


The description of generated actor
#######################################################################################################################

The description of **what** to build, **how** to build it, and **where** to build it is passed to the generator methods 
through the ``actor_settings`` Python dictionary. This data structure contains three main entries (or sub-dictionaries) 
at its root:

* ``actor_description`` - Describes the actor's parameters (see details in the :ref:`actor description section 
  <yaml_actor_description_anchor>`).
* ``code_description`` - Describes the code to be wrapped (see details in the :ref:`code description section 
  <yaml_code_description_anchor>`).
* ``platform_settings`` - Contains default values for the given platform/installation (see details in the 
  :ref:`platform settings section <yaml_platform_settings_anchor>`).

.. note::
    The ``actor_settings`` data can be accessed as a standard Python dictionary. It is up to the developer to decide 
    which entries to use and how to use them.