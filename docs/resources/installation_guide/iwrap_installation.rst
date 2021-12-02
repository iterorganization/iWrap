#######################################################################################################################
Installation process
#######################################################################################################################


.. toctree::

iWrap requirements
#######################################################################################################################

**To build and install iWrap, the following requirements must be met:**

* **Installation requires Python 3.7 or later**
* **Installation requires setuptools 43.0 or later and wheel**
    .. code-block:: shell

       python3 -m pip install --upgrade pip
       python3 -m pip install --upgrade setuptools>=43 wheel
    
* **Additionaly, installation requires following modules:**

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

After a successful installation, it is recommended to clean the root of the project from compilation residues. To do this, run a cleaning recipe:

.. code-block:: shell

   make clean

Extras
#######################################################################################################################

**1. Uninstall iWrap:**

Proper uninstallation of iWrap (including its distribution and module file) requires the use of the same INSTALL_DIR and INSTALL MOD variables as in the installation process.

* Run the uninstall recipe:

.. code-block:: shell
   
   export INSTALL_DIR=[DIRECTORY...]
   export INSTALL_MOD=[DIRECTORY...]
   
   make uninstall

**Remember that from the given directories only the current tagged version will be uninstalled!**

**2. Verification of the prepared installation configuration:**

After setting the INSTALL_DIR and INSTALL_MOD variables, the pre-installation configuration can be verified using the help recipe:

.. code-block:: shell
   
   export INSTALL_DIR=[DIRECTORY...]
   export INSTALL_MOD=[DIRECTORY...]
   
   make help

This will automatically evaluate all paths and print them with the appropriate descriptions.

**3. Adding the module file to the MODULEPATH variable:**

If the INSTALL_MOD directory is not already attached to MODULEPATH, it can simply be added manually:

.. code-block:: shell
   
   module use INSTALL_MOD

**4. Diagnostics:**

* After installing the module and adding it to MODULEPATH, check if it is available

.. code-block:: shell
   
   module avail iwrap

* If so, load the module into the environment:

.. code-block:: shell
   
   module load iwrap

* Check that iWrap has loaded properly and that the Python package manager is able to find it:

.. code-block:: shell
   
   python3 -m pip list

and look for iwrap package,

or:

.. code-block:: shell
   
   python3 -m pip list | grep iwrap

**5. Run iWrap:**

Load the module into the environment:

.. code-block:: shell
   
   module load iwrap

* Command line:

.. code-block:: shell
   
   iwrap

* GUI:

.. code-block:: shell
   
   iwrap-gui

