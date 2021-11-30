#######################################################################################################################
Installation process
#######################################################################################################################


.. toctree::

iWrap requirements
#######################################################################################################################

**To build and install iWrap, the following requirements must be met:**

* *IMAS:*
.. code-block:: shell

   module load IMAS

* *LXML:*
.. code-block:: shell

   module load lxml

* *XMLLIB:*
.. code-block:: shell

   module load XMLlib

* *tomli:*
.. code-block:: shell

   python3 -m pip install --upgrade --user tomli

Installation
#######################################################################################################################

**1. Build an installable iWrap distribution:**

* Navigate to the project's root directory and run:

.. code-block:: shell

   make iwrap_build

**2. Prepare the installation directory path and the modulefile installation dir:**

* Installation directory:

.. code-block:: bash

   export INSTALL_DIR=[DIRECTORY...]

where DIRECTORY is the absolute path to the installation directory. 

A new folder will be created in the specified directory path with a name based on the current version tag.

*  Modulefile installation directory:

.. code-block:: bash

   export INSTALL_MOD=[DIRECTORY...]

where DIRECTORY is the absolute path to the directory where the module file should be placed.

A new subdirectory named as project will be created in this directory, 
and inside it a module file named after the current version tag will be placed.

**3. Install:**

* Run installation recipe:

.. code-block:: shell

   make install

If the installation is successful, the following information will be printed:

.. code-block:: bash

        IWRAP_INSTALL FINISHED
        iWrap installed in: [INSTALL_DIR/VERSION]  
    and
        INSTALL_MODULE FINISHED
        iWrap ENVIRONMENT MODULE installed in: [INSTALL_MOD/MODULEFILE]