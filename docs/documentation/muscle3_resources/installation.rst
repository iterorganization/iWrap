.. sectnum::

.. toctree::

Installer functionality
#######################################################################################################################

iWrap MUSCLE3 plugins installer:

- Builds WWW pages with manuals from ReStructured Text files using Sphinx
- Copies manuals to chosen installation folder
- Copies plugins (actor generators with set of templates) to chosen installation folder
- Builds module file from template and install it in selected directory

Requirements
#######################################################################################################################

Software required to install plugins:

- `make` >= 3.82

Software required to build and install manuals:

- `make` >= 3.82
- `Python` >= 3.8.6
- Python packages:

  - `Sphinx` >= 3.2.1
  - `sphinx-bootstrap-theme` >= 0.7.1
  - `sphinx-rtd-theme` >= 1.0.0

Installation
#######################################################################################################################

iWrap MUSCLE3 plugins installer is based on `make` utility. To check all available targets
and configuration options, `make help` could be run from the console.

.. code-block:: console

    shell> make help

    Available targets:
      install                       : install plugins and iWrap4paf module (default)
      uninstall                     : uninstall iWrap4paf module
      install_plugins               : install iWrap4paf plugins
      install_module                : install iWrap4paf module
      clean                         : remove installation directories
      help                          : display this help message

    Variables:
      IWRAP_MODULE_NAME             : name of iWrap module [ iwrap ]
      MUSCLE3_MODULE_NAME           : name of MUSCLE3 module [ muscle3 ]
      VERSION                       : version of iWrap4paf [ 0.1.0 ]
      NAME                          : name of iWrap4paf module [ iwrap4paf ]
      INSTALL_PREFIX                : path to software installation directory [ $HOME/IWRAP4PAF/install ]
      INSTALL_DIR                   : full path to iWrap4paf installation directory [ $HOME/IWRAP4PAF/install/iwrap4paf/0.1.0 ]
      MODULE_NAME                   : name of module file [ iwrap4paf ]
      MODULE_INSTALL_PREFIX         : path to module installation directory [ $HOME/IWRAP4PAF/modules ]
      MODULE_INSTALL_DIR            : full path to module installation directory [ $HOME/IWRAP4PAF/modules/iwrap4paf/0.1.0 ]


.. note::
   A command ``make help`` shows not only systems variables that may be configured,
   but also their current values. It is strongly recommended to check before installation,
   if all variables have correct (wanted) values.


Plugins installation
=========================================================================================
To install the plugins a `make install_plugins` command should be executed.

.. code-block:: console

    shell> make install_plugins

The installer copies plugins files to a directory defined as:

.. code-block:: console

   $INSTALL_PREFIX/$NAME/$VERSION

As it could be easily spotted, the target installation directory could be changed
by setting following system variables:

- ``INSTALL_PREFIX`` - software install direcotry
- ``NAME`` - name of the plugins pack (may differ on various platforms)
- ``VERSION`` - plugins version (default value is set automatically using ``git describe``)

Module installation
=========================================================================================
The module could be installed by launching `make install_module` command.

.. code-block:: console

    shell> make install_module

The installer builds a module file from the template based on a current values of following system variables:

- ``IWRAP_MODULE_NAME`` - name of iWrap module (may differ on different platforms)
- ``MUSCLE3_MODULE_NAME`` - name of MSUCLE3 module (may differ on different platforms)
- ``VERSION`` - plugins version (default value is set automatically using ``git describe``)
- ``MODULE_NAME`` -  name of module (may differ on different platforms)
- ``MODULE_INSTALL_PREFIX``- path to software modules directory
- ``MODULE_INSTALL_DIR`` -  full path to module installation directory

Verification
#######################################################################################################################

Verification of the plugins installation
=========================================================================================

Once plugins are installed, correctness of the installation can be verified by listing
available actor types registered in iWrap:

.. code-block:: console

    shell> iwrap --list-actor-types

             Id          :              Name              : Description
    ----------------------------------------------------------------------
           python        :      Simple Python actor       : Simple Python actor
       MUSCLE3-Python    :        MUSCLE3 (Python)        : Wrapping Python code into MUSCLE3 micro model
        MUSCLE3-CPP      :         MUSCLE3 (C++)          : Wrapping C++ code into MUSCLE3 micro model
      MUSCLE3-Fortran    :       MUSCLE3 (Fortran)        : Wrapping Fortran code into MUSCLE3 micro model

.. note::
   Please make sure that paths to all installed plugins are added to ``PYTHONPATH`` including directory containing
   common functionality (it is usually done by loading module `iwrap-plugins-muscle3`). Typical configuration usually
   looks like follows:

    .. code-block:: shell

        # <INSTALL_DIR> is the directory where plugins were installed
        export PYTHONPATH=<INSTALL_DIR>/generators/common:${PYTHONPATH}
        export PYTHONPATH=<INSTALL_DIR>/generators/fortran:${PYTHONPATH}
        export PYTHONPATH=<INSTALL_DIR>/generators/cpp:${PYTHONPATH}
        export PYTHONPATH=<INSTALL_DIR>/generators/python:${PYTHONPATH}


Verification of the module installation
=========================================================================================

After installing the module and adding it to MODULEPATH, its availability and correctness
could be checked by listing and loading `iwrap-plugins-muscle3` module

.. code-block:: shell

   # <MODULE_NAME> is the name chosen for the module at the installation stage (default: iwrap-plugins-muscle3)
   module available <MODULE_NAME>
   module load <MODULE_NAME>



Uninstall
#######################################################################################################################

To uninstall software following commands could be run:

.. code-block:: shell

   make unistall             # to uninstall plugins and module
   # OR
   make uninstall_plugins    # to uninstall plugins
   make uninstall_module     # to uninstall module


.. warning::

    Proper uninstallation of plugins (including their distribution and module file) requires the use
    of exactly (!) the same system variables as for the installation process.





