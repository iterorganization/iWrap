#######################################################################################################################
iWrap internal design
#######################################################################################################################

Design Principles
=================

The iWrap framework is built on a modular and open architecture, designed to maintain a clear separation between data, logic, and presentation layers. It features a configurable actor layer stack, where each layer is distinctly separated and abstracted. This design allows for dynamic construction and reconfiguration of actors by enabling layers to be swapped out or replaced as needed. The system supports plug-ins to extend functionality, ensuring that additional features can be seamlessly integrated. Data handling is standardized across modules, providing consistent processing and enhancing the flexibility and adaptability of iWrap.

.. figure:: /images/iwrap_architecture.png
  :width: 600
  :alt: iWrap internal design

  iWrap internal design


User interfaces
=================

Users can interact with iWrap in two ways: through the command-line interface (CLI) or the graphical user interface (GUI).

CLI: Command Line Interface
---------------------------

The command-line interface is designed for advanced users and utilizes pre-configured YAML descriptions of actors. The CLI is particularly useful for automating the actor generation process, enabling users to launch this process from scripts or other automated systems.

.. hint::

    * Refer to the :ref:`iWrap CLI<iWrap CLI>` section for detailed instructions on how to use the iWrap CLI.
    * The command-line interface is implemented in the :py:mod:`iwrap.iwrap_main` module.


GUI: Graphical User Interface
-------------------------------
The iWrap GUI is intended for less experienced users, providing a more intuitive way to specify actor descriptions and generate the corresponding code compared to the CLI.

.. hint::
    * Detailed description of the iWrap GUI and its functionality could be found :ref:`here <iWrap GUI>`
    * Graphical interface consists of classes that belongs to package :py:mod:`iwrap.gui`


Actor Generation Engine
========================

`The Actor Generation Engine` is the core component of iWrap, orchestrating the actor generation process. It manages lists of generators for each layer, specialized for handling specific programming languages, data types, or custom logic. When the actor generation process is triggered via the GUI or CLI, the engine selects one generator per layer and sequentially invokes its methods to generate source code, build the code, and install the actor layer. The choice of generator is determined by the actor type, the programming language, and the metadata associated with the generators.

.. hint::

   * The implementation of the engine could be found in the package :py:mod:`iwrap.generation_engine`


Actor Generators
========================
A 'generator' is a component responsible for creating, building, and installing the code for a specific actor layer, based on the actor type and programming language.

To create an actor, the generator(s) require the following inputs:

* An actor description, sourced from either a YAML file or internal GUI structures.
* Information about the current platform (e.g., cluster) where the actor is being generated.
* The user code that needs to be wrapped.

Generators are managed via a plug-in mechanism that allows for a high degree of configurability within specific iWrap installations. Some generators (such as those used to create simple Python actors) are built-in, while others (like the set of MUSCLE3 actor generators) can be loaded or unloaded according to user requirements. This topic is discussed in greater detail in subsequent chapters.
