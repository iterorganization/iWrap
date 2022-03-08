#######################################################################################################################
Actor usage and configuration
#######################################################################################################################

.. toctree::
   :numbered:
   :maxdepth: 10


Importing an actor class
######################################################################################################################

To make an actor class accessible inside a workflow script, an actor class has to be imported:

.. code-block:: Python

 from <actor_package>.actor import <actor_class>

Both: *<actor_package>* and *<actor_class>*  are set to the same value, provided by user as an *'actor name'.*
To import an actor named e.g. *'physics_ii*' a correct import will look like:


.. code-block:: Python

     from physics_ii.actor import physics_ii

.. warning::
    To be able to import any actor, directory containing actors has to be added to *PYTHONPATH*

    .. code-block:: bash

        export PYTHONPATH=</path/to/actors/directory>:$PYTHONPATH



Actor life cycle
######################################################################################################################

During its 'life' an actor goes through several states, that have to be passed
only in a strict order:

- Actor creation
- Configuration of runtime settings
- Initialisation
- Calling the main ('step') subroutine
- Finalisation

**Actor class API**

.. code-block:: Python

    class actor_class_name:

       def get_runtime_settings(self) -> RuntimeSettings:
           ...

       def get_code_parameters(self) -> CodeParameters:
           ...

       def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:
           ...

       def run(self, *args)->None:
           ...

       def __call__(self, *args):
           return self.run( *args )

       def finalize(self) -> None:
           ...

Creation of the actor object
=========================================================================================

An actor instance can be created, using already imported actor class, in usual 'pythonic' way:

.. code-block:: Python

     actor_object = <actor name>()
     e.g.
     actor_object = physics_ii()

Configuration of an actor
=========================================================================================

Workflow developer can (re)configure an actor before its initialisation and native code execution.

Settings that can be changed include:

- Code parameters (see chapter `Physics model parameters`_ for details)
- Runtime settings (see chapter `Actor runtime settings`_ for details)

.. warning::
   - An access to both: *runtime settings* and *code parameters* is **restricted**. They cannot be accessed directly
     as an ordinary class attributes but using specialized getters ( ``get_runtime_settings()``
     and ``get_code_parameters()``)
   - ``initialize(...)`` method of an actor must be called after any change of *runtime settings* or *code parameters*

Actor initialisation:
=========================================================================================

The ``initialize(...)`` method of the actor to perform internal initialisation actions:

.. code-block:: Python

   actor_object.initialize(runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:

This method:

* Updates an actor runtime settings
* Updates native code parameters
* Validate code parameters
* Initialises *sandbox*:

  * Creates sandbox directory (if it doesn't exists)
  * Clean up the content of sandbox directory (if it was previously created)

* Initialises IDS temporary storage (used for passing IDSes to the native code)
* Calls *initialisation* method of the native code
* Typically this method should be run when workflow starts


Main method call:
=========================================================================================

At this stage an actor call *main* method of the native code:

- This step can be repeated an arbitrary number of times
- A list of inout/output IDS object has to be passed to method
- Actor *main* method can be called:

  - Implicitly (the ``__call__(...)`` method is implemented so the method can be run
    using object_name only (suggested method!):


    .. code-block:: Python

       <output IDS or list of IDSes> = actor_object(<input IDS/IDSes>)
       e.g.
       output_distribution_sources = actor_object(input_core_profiles)

  - Explicitly, using ``run(...)`` method:

    .. code-block:: Python

       <output IDS or list of IDSes> = actor_object.run(<input IDS/IDSes>)
       e.g.
       output_distribution_sources = actor_object.run(input_core_profiles)

- Actor sandbox directory is cleaned up (depending on `Sandbox settings`_)

Actor finalisation
=========================================================================================

.. code-block:: Python

   actor_object.finalize()

This method of the actor is usually used to perform internal finalisation actions (clean up the environment etc):

- Calls of a *finalize* method of the native code
- Cleans up IDS temporary storage
- Actor sandbox directory is cleaned up (depending on `Sandbox settings`_)
- Actor sandbox directory is removed (depending on `Sandbox settings`_)
- Typically this method should be run when workflow finishes its execution

Physics model parameters
######################################################################################################################

Native code parameters, which default value is provided (as a file path) while generating an actor,
can be change in runtime

-   *code_parameters*  cannot be accessed directly, but only via a special getter method:

    .. code-block:: Python

       def get_code_parameters(self) -> CodeParameters:
           ...

-   Attributes:

    - *parameters*: string; XML parameters; Read only attribute
    - *schema*: string; XML schema used for XML validation; Read only attribute
    - *parameters_path*: string; path to XML file; can be set be the user to overwrite default XML parameters

-   Methods:

    -   ``def get_parametr_value(self, path_to_node:str) -> str:`` - gets value of XML node
        described by ``path/to/node``

    -   ``def set_parametr_value(self, path_to_node:str, value:str) -> str:`` - sets ``value`` to XML node
        described by ``path/to/node``

- Code parameters are validated while calling actor ``initialize`` method

- Example of the usage:

.. code-block:: Python

        # gets code parameters
        code_parameters = actor_object.get_code_parameters()
        #overwrites default value
        code_parameters.parameters_path= '/gss_efgw_work/scratch/g2bpalak/tmp/xml_new_location.xml'
        # checks value of node
        value = code_parameters.get_parametr_value('parameters/multiplication_factor')
        # sets value of node
        code_parameters.set_parametr_value( 'parameters/multiplication_factor', 0.5 )

        # updates (and validates) parameters
        actor_object.initialize(code_parameters=code_parameters)

.. _actor_settings_anchor:

Actor runtime settings
######################################################################################################################

The ``runtime_settings`` property tells the actor how native code should be run and defines:

-   Run mode
-   Debug settings
-   MPI settings
-   Batch settings
-   Temporary IDS storage settings
-   Command line to be run
    
*runtime_settings*  cannot be accessed directly, but only via a special getter method:

.. code-block:: Python

   def get_runtime_settings(self) -> RuntimeSettings
       ...

Actor runtime settings, to be updated must be passed as ``initialization`` argument

- Example of the usage:

.. code-block:: Python

        # gets runtime settings
        runtime_settings = actor_object.get_runtime_settings()

        #configures runtime settings
        runtime_settings.attribut.to_be.set = value

        # updates runtime_settings
        actor_object.initialize(runtime_settings=runtime_settings)

Run mode
=========================================================================================

-   Defined by setting one of predefined ``RunMode`` enumeration class values

-   ``RunMode.NORMAL`` (default) - native code is loaded from system library and called directly from Python,
    within the same process (and environment) that workflow script. Usually system resources,
    shared with other Python threads are limited, however this mode is suitable for most of the actors.

-   ``RunMode.STANDALONE``   - an actor runs native code as executable in a separate system process, having its
    own environment and (usually) bigger system resources available. This mode is set automatically for MPI
    applications, however it can be set automatically e.g. for memory demanding code.

-   ``RunMode.BATCH`` - an actor standalone executable is submitted to a batch queue.
    See `Batch settings`_ for details concerning batch job configuration

-  Import of enumerated values:

   .. code-block:: Python

       from <actor name>.common.runtime_settings import RunMode

-  Example of the usage:

   .. code-block:: Python

       from <actor name>.common.runtime_settings import RunMode

       ...
       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       #configures runtime settings
       runtime_settings.run_mode = RunMode.STANDALONE

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)

Debug mode
=========================================================================================

-   Defined by setting one of predefined ``DebugMode`` enumeration class values

-   DebugMode.STANDALONE   - similarly to STANDALONE *run mode* - an actor runs *native code as executable
    in a separate system process*, but this time under debugger control. Debugged code can be run several
    times. To proceed with workflow execution is enough to close the debugger. This debugging mode is suitable
    for most of the purposes.

-   DebugMode.ATTACH   - an actor runs a debugger as parallel process, attaching it to a running workflow
    and setting breakpoint on wrapped native code of the debugged actor.  Because debugger attaches to a
    workflow (and not a particular actor) killing debugged process kills the whole workflow. This mode has to be
    chosen if the issue within code cannot be reproduced in STANDALONE mode and the issue results from actor
    interdependencies (e.g. one actor overwrites memory of the other one).

-   Import of enumerated values:

    .. code-block:: Python

      from <actor name>.common.runtime_settings import DebugMode

-   See `Batch settings`_ for details concerning batch job configuration


-   Example of the usage:

   .. code-block:: Python

       from <actor name>.common.runtime_settings import DebugMode

       ...
       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       #configures runtime settings
       runtime_settings.run_mode = DebugMode.STANDALONE

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)


MPI settings
=========================================================================================

-   *mpi_nodes* - number of MPI nodes to be used (default 1)

-   *mpi_default_runner* - default MPI runner (``mpiexec``, ``mpirun``, etc) to be used. Its value is platform dependent,
    read from iWrap configuration. Read only attribute

-   *mpi_runner* - user defined MPI runner to be used. If not provided *mpi_default_runner* is being used

-   *mpi_default_options* - default MPI runner options to be added to command line. Its value is platform dependent,
    read from iWrap configuration. Read only attribute

-   *mpi_options* - user defined options for MPI runner (e.g. ``-tv`` or ``--debug`` for debugging).

-   Example of the usage:

   .. code-block:: Python

       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       #configures runtime settings
       runtime_settings.mpi.mpi_nodes = 4
       runtime_settings.mpi.mpi_runner = 'mpirun'
       runtime_settings.mpi.mpi_options = '-tv'

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)

.. note::
   -  MPI code is *always* run as executable in standalone or batch mode
   -  If a native code is not marked as 'MPI' (i.e. MPI compiler is not set)  during actor generation,
      mpi settings are ignored
   -  MPI commandline is built by concatenating:

      mpi_executable = ``mpi_runner`` + ``mpi_default_options`` + ``mpi_options`` + ``executable``

Batch settings
=========================================================================================

-   *batch_nodes* - number of nodes to be used (default 1)

-   *batch_default_runner* - default batch runner (``sbatch``, ``srun``, etc) to be used. Its value is platform dependent,
    read from iWrap configuration. Read only attribute

-   *batch_runner* - user defined batch runner to be used. If not provided *batch_default_runner* is used

-   *batch_default_options* - default batch options to be added to command line. Its value is platform dependent,
    read from iWrap configuration. Read only attribute

-   *queue* - batch queue to be used

-   *batch_options* - user defined options for batch runner

-   Example of the usage:

   .. code-block:: Python

       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       #configures runtime settings
       runtime_settings.batch.batch_nodes = 4
       runtime_settings.batch.batch_runner = 'srun'

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)

.. note::
   -  Batch commandline is built by concatenating:

      batch_executable = ``batch_runner`` + ``batch_default_options`` + ``batch_options`` + ``executable``

      If an actor is MPI ``executable``   is ``mpi_executable`` (see above)


Sandbox settings
=========================================================================================

"Sandbox" - a directory, in which actor will be run.
Before execution of user codes wrapped by an iWrap generated actor, directory will be changed to "sandbox",
and after actor finishes, current directory will be switched back to previous value. The name (path) of "sandbox"
directory will be created automatically or has to be specified by user.

Sandbox attributes:

-   *mode* - defines how sandbox is managed. One of the predefined values of class SandboxMode:

    -   ``SandboxMode.MANUAL`` - full manual mode. It is developer responsibility to maintain sandbox
        (i.e. create it, clean it up, etc), Requires *path* attribute to be set.

    -   ``SandboxMode.AUTOMATIC`` - iWrap generated actor manages the sandbox creation, clean up, etc

-   *path* - a **valid** path to an existing directory, that in 'manual' mode will be used as a sandbox.
    In 'automatic' mode directory is created by an actor...

-   *life_time* - defines when the sandbox will be cleaned up and removed. One of the predefined values
    of the class SandboxLifeTime:

    -   ``SandboxLifeTime.ACTOR_RUN`` - content of the sandbox directory is cleaned before and after
        every main actor method execution.

    -   ``SandboxLifeTime.WORKFLOW_RUN`` - content of the sandbox directory is cleaned, during initialising stage
        of an actor and after other finalisation actions of the actor (so, sandbox should be available during
        the whole workflow run)

    -   ``SandboxLifeTime.PERSISTENT`` - content of the sandbox directory is preserved and never cleaned up

-   Import of enumerated values:

    .. code-block:: Python

      from <actor name>.common.runtime_settings import SandboxLifeTime, SandboxMode

-   Example of the usage:

   .. code-block:: Python

       from <actor name>.common.runtime_settings import SandboxMode

       ...
       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       #configures runtime settings
       runtime_settings.sandbox.mode = SandboxMode.MANUAL
       runtime_settings.sandbox.path = '/path/to/existing/sandbox/directory'

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)


IDS storage settings
=========================================================================================

This attribute defines settings of temporary storage being used while passing IDSes between an actor and native code.

-  Storage parameters that can be set:

   -   db_name:

       -  Meaning: name of data base to be used
       -  Default value: 'tmp'

   -   backend:

       -  Meaning - backend to be used
       -  Default value -   ``imas.imasdef.MEMORY_BACKEND``

   -    persistent_backend

        -  Meaning - backend to be used when temporary data cannot be stored in memory (e.g. while running
           actor in a standalone mode, when a native code is run as separate process, so it doesn't share
           memory with other actors.
        -  Default value -  ``imas.imasdef.MDSPLUS_BACKEND``

.. note::
   Please note: for most of the purposes it is fine to not set this property and leave default values unchanged.

User defined commandline command
=========================================================================================
Workflow developer may take full control on the way an actor is run defining ``commandline_cmd``
attribute of ``runtime_settings``. If not set, the automatically generated commandline will be used
(typical for the most of the usage scenarios)


.. _command_tag_usage_anchor:
.. info::
   User defined commandline may contain predefined tags (syntax ``${tag}``), where ``tag``
   is the name of one of the runtime_settings attributes. The usage of tags is optional.
   The only exception is ``${exec}`` tag (representing full path to executable binary),
   which MUST be present in the commandline.




The workflow example
######################################################################################################################

.. _workflow_example_anchor:

.. code-block:: python

    import sys
    import imas,os

    from physics_ii.actor import physics_ii
    from physics_ii.common.runtime_settings import RunMode, DebugMode, SandboxLifeTime



    class ExampleWorkflowManager:

        def __init__(self):

            self.actor_physics_ii = physics_ii()
            self.input_entry = None
            self.output_entry = None

        def init_workflow(self):
            # INPUT/OUTPUT CONFIGURATION
            shot                = 131024
            run_in              = 1
            input_user_or_path  = 'public'
            input_database      = 'iter'
            run_out             = 10
            output_user_or_path = os.getenv('USER')
            output_database     = input_database

            # OPEN INPUT DATAFILE TO GET DATA FROM IMAS SCENARIO DATABASE
            print('=> Open input datafile')
            self.input_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,input_database,shot,run_in,input_user_or_path)
            self.input_entry.open()

            # CREATE OUTPUT DATAFILE
            print('=> Create output datafile')
            self.output_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,output_database,shot,run_out,output_user_or_path)
            self.output_entry.create()

            runtime_settings = None
            # # # # # # # # Initialization of ALL actors  # # # # # # # #

            runtime_settings.debug_mode = DebugMode.STANDALONE
            runtime_settings.sandbox.life_time  = SandboxLifeTime.PERSISTENT

            code_parameters = self.actor_physics_ii.get_code_parameters()
            value = code_parameters.get_parametr_value('parameters/multiplication_factor')
            code_parameters.set_parametr_value( 'parameters/multiplication_factor', 0.5 )
            self.actor_physics_ii.initialize(runtime_settings=runtime_settings, code_parameters=code_parameters)

        def execute_workflow(self):
            # READ INPUT IDSS FROM LOCAL DATABASE
            time_slice          = 200.
            print('=> Read input IDSs')
            input_equilibrium = self.input_entry.get_slice('equilibrium', time_slice, 1)

            # EXECUTE PHYSICS CODE
            print('=> Execute physics code')
            output_equilibrium = self.actor_physics_ii(input_equilibrium)

            # SAVE IDSS INTO OUTPUT FILE
            print('=> Export output IDSs to local database')
            self.output_entry.put(output_equilibrium)
            print('Done exporting.')

        def end_workflow(self):

            # Finalize ALL actors
            self.actor_physics_ii.finalize()

            #other finalization actions
            self.input_entry.close()
            self.output_entry.close()

    manager = ExampleWorkflowManager()

    manager.init_workflow()
    manager.execute_workflow()
    manager.end_workflow()


