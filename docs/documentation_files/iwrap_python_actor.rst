iWrap Python Actor
==================

Use cases
---------

Wrapper modes:
~~~~~~~~~~~~~~

* interactive - user code is run directly from wrapper
* batch - user executable is sent to a queue

Execution modes
~~~~~~~~~~~~~~~

User code can be run in mode:

* Sequential
* Parallel:
    * MPI
        * MPICH
        * OpenMPI
    * and/or OpenMP

Debugging modes:
~~~~~~~~~~~~~~~~
* standalone - debugger launches executable containing user code in a separate process
* attach - debugger attaches to running process executing user code

Sandbox
~~~~~~~
**"Sandbox"** - a directory, in which actor will be run. Before execution of user codes wrapped by FC2K generated actor, directory will be changed to "sandbox", and after actor finishes, current directory will be switched back to previous value. The name (path) of "sandbox" directory will be created automatically or specified by user in actor configuration dialog.

Actor will use existing directory or will create it, if directory not exists.

Typical usage: checkpointing, caching intermediate results,  usage of additional information (input, config files) not provided in workflow.

Actor API - current status
--------------------------

.. code:: python

    def actor_name(integer0, integer1, double0, double1, core_profiles0, core_profiles1, equilibrium0, exec_type='mpi_local', mpi_process = 4, strip_output = True):

Input arguments required by user C++/F method
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Number and an order of arguments must  be exactly the same as in user method
* Types of arguments
    * IDS
    * Primitive type
    * Array of primitive types
    * String

Execution type
~~~~~~~~~~~~~~

* Named argument (exec_type='').
* Current values of exec_type :
    * ctypes - interactive execution (yes - this name is REALLY CONFUSING)
    * mpi_local - run as MPI job
    * dbg - to keep compatibility with obsolete 'debug' argument of wrapper

Additional arguments
~~~~~~~~~~~~~~~~~~~~

Auxiliary keyword arguments:
* mpi_processes  - integer - number of MPI processes
* lib_opt  - boolean - determines of alternative user library should be used, instead of the default one
* get_output_idss  - boolean - determines, what actually will be returned if returned argument is IDS: an IDS object, or metadata describing it
* strip_output  - boolean - it determines (if returned argument is a string) it it should be stripped or not
* dbgmode  - boolean - unknown purpose

Returned values
~~~~~~~~~~~~~~~

Output values:
* None - if nothing is returned from called F/C++ method
* A single object -  if called method returns only one value
* A tuple of objects - if called methods returns more than one value. It contains also diagnostic_info if being used.

Weaknesses of current solution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* very long list of arguments - it is easy to make a mistake:
    * providing  incorrect number of arguments
    * providing incorrect order of arguments
* lack of important information, so many features are not handled:
    * no debug mode
    * MPI parameters - only number of nodes is specified
    * no batch mode


Actor API - new design
----------------------

.. code::

    class <ActorName>Actor:
        def __init__(self):
            self.job_settings = IWrapJobSettings()
            self.parameters = IWrapActorParameters()

        # # #  Actor lifecycle methods # # #
        def initialize(self):
            pass

        def fire(self, arguments):
            pass

        def wrapup(self):
            pass

