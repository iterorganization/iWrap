iWrap Architecture
==================

.. figure:: /resources/iwrap_architecture.png
   :align: center

   *iWrap Architecture*

Components
----------

Actor Description:
~~~~~~~~~~~~~~~~~~

    * Data structure storing information about the actor:
        * Actor name
        * Location of user library/libraries
        * Name user subroutine being wrapped
        * Arguments in/out
        * Compilers being used
        * etc, etc
    * Data structure -  a tree of Python classes
    * Actor description can be saved to file for the future use (aka "project file") and then read from it
    * Actor description can be displayed and modified by various GUI or CLI tools (within the scope of the contract only FC2K-like GUI will be implemented)
    * Actor description are used as source of information for generation of actor (actor files)

System Information:
~~~~~~~~~~~~~~~~~~~

* Data structure storing information about underlying system/platform features
* Information handled:
* Available compilers
* Available MPI/OpenMPI libraries
* Template of batch submission script
* Available debuggers
* etc, etc

Graphical User Interface:
~~~~~~~~~~~~~~~~~~~~~~~~~

* FC2K-like GUI
* GUI allows:
    * to display actor data
    * to modify actor data
    * to launch generation of actor
    * to show the return status of generation (OK, Error)
* Some information will be suggested to the user, based on System Settings (e.g. list of available compilers)

Command Line Interface:
~~~~~~~~~~~~~~~~~~~~~~~

* Very simple script
* CLI allows:
    * to launch generation of actor based on existing actor project file
    * to show the return status of generation (OK, Error)
* Actor Generation Engine:
    * AGE manages Actor Generator Plugins (TBD)
    * Based on user choice (action), fires  appropriate Actor Generator Plugin
* Actor Generator Plugin:
    * AGP, based on actor description, generates actor files

.. figure:: /resources/iwrap_example_of_layers.png
   :align: center

   *iWrap Example of layers*

.. figure:: /resources/plugin_management.png
   :align: center

   *Generators' Plug-in Management*
