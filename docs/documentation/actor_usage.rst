#######################################################################################################################
Actor usage and configuration
#######################################################################################################################

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

       def get_state(self) -> str:
           ...

       def set_state(self, state: str) -> None:
           ...

       def get_timestamp(self) -> float:
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

Workflow developer can (re)configure an actor before its initialisation and the execution of the code.

Settings that can be changed include:

- Code parameters (see chapter `Code-specific parameters`_ for details)
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

* Updates the actor runtime settings
* Updates the code specific parameters
* Validate the code parameters
* Initialises the *sandbox*:

  * Creates the sandbox directory (if it doesn't exists)
  * Clean up the content of the sandbox directory (if it was previously created)

* Initializes IDS temporary storage (used for passing IDSes from the actor to the code)
* Calls the *initialization* method of the code
* Typically this method should be run at the start of the workflow 


Main method call:
=========================================================================================

At this stage an actor call the *main* method of the code:

- This step can be repeated an arbitrary number of times
- A list of input/output IDS objects has to be passed to the method
- The actor *main* method can be called:

  - Implicitly (the ``__call__(...)`` method is implemented so the method can be run
    using object_name only (preferred  method!):


    .. code-block:: Python

       <output IDS or list of IDSes> = actor_object(<input IDS/IDSes>)
       e.g.
       output_distribution_sources = actor_object(input_core_profiles)

  - Explicitly, using ``run(...)`` method:

    .. code-block:: Python

       <output IDS or list of IDSes> = actor_object.run(<input IDS/IDSes>)
       e.g.
       output_distribution_sources = actor_object.run(input_core_profiles)

- The actor sandbox directory is cleaned up (depending on `Sandbox settings`_)

Actor finalization
=========================================================================================

.. code-block:: Python

   actor_object.finalize()

This method of the actor is usually used to perform any internal finalization actions in the code (clean up the environment etc):

- Calls of the *finalize* method of the code
- Cleans up IDS temporary storage
- The actor sandbox directory is cleaned up (depending on `Sandbox settings`_)
- The actor sandbox directory is removed (depending on `Sandbox settings`_)
- Typically this method should be run when at the end of the workflow

Additional actor methods
######################################################################################################################

Code restarting methods
=========================================================================================

.. code-block:: Python

   code_state : str = actor_object.get_state()

   actor_object.set_state(code_state)


The actor ``get_state`` and  ``set_state`` methods enable restart stateful, sometimes compute demanding,
codes without losing intermediate results that are not captured in the code's outputs. The code may be asked periodically
about its internal state using the ``get_state`` method. After a restart, the code state can be restored
using the ``set_state`` method.

The internal state of the code has to be passed as a string, however iWrap gives full flexibility
to the code developer concerning the format and content of state description.
It is a kind of a ‘black box’ returned from ``get_state`` and passed to ``set_state`` method during restart,
so the only requirement is that information returned by ``get_state`` is understandable by ``set_state``.

- Example of usage:

.. code-block:: Python

        # ACTOR INITIALIZATION
        ...
        # restore the code state if file keeping state exists
        if file_exists('code_state.txt'):
            with open( 'code_state.txt', 'r' ) as file:
                code_state = file.read()

            # Starting from the saved code state
            self.actor.set_state(code_state)
        else:
            # Starting from the begin

        # COMPUTATIONS
        ...

        # ACTOR FINALIZATION
        ...
        # Getting internal code state
        code_state = self.actor.get_state()

        # save code state to file
        with open( 'code_state.txt', 'w' ) as file:
            file.write(code_state)


Get timestamp method
=========================================================================================

.. code-block:: Python

   timestamp : float = actor_object.get_timestamp()

``get_timestamp`` method allows to obtain currently computed physical time.
Such information can help support consistent physical time handling throughout the coupled simulation.

Code-specific parameters
######################################################################################################################

Code-specific parameters, for which the default value is given (as a file path) when generating the actor,
can be change at runtime

-   *code_parameters*  cannot be accessed directly, but only via a special getter method:

    .. code-block:: Python

       def get_code_parameters(self) -> CodeParameters:
           ...

-   Attributes:

    - *parameters*: string; [XML, JSON, Namelist] parameters; Read only attribute
    - *schema*: string; [XML, JSON, Namelist] schema used for parameters validation; Read only attribute
    - *parameters_path*: string; path to parameters file; can be set be the user to overwrite default parameters

-   Methods:

    -   ``def get_parameter(self, path_to_node:str) -> str:`` - gets value of parameters node
        described by ``path/to/node``

    -   ``def set_parameter(self, path_to_node:str, value:str) -> str:`` - sets ``value`` to parameters node
        described by ``path/to/node``

- Code parameters are validated while calling actor ``initialize`` method

- ``path_to_node`` contains parameters node names separated by ``/`` character.

- One can access n-th node from group of nodes by using ``()`` operator i.e. ``code_parameters.get_parameter('parameters/multiplication_factor(3)')`` (counting from 0)

- ``set_parameter`` method can take as argument any object convertable to ``str``, or any list of objects convertable to ``str``. List will be converted to space-separated string value

- Example of the usage:

.. code-block:: Python

        # gets code parameters
        code_parameters = actor_object.get_code_parameters()
        #overwrites default value
        code_parameters.parameters_path= '/gss_efgw_work/scratch/username/tmp/xml_new_location.xml'
        # checks value of node
        value = code_parameters.get_parameter('parameters/multiplication_factor')
        # sets value of node
        code_parameters.set_parameter( 'parameters/multiplication_factor', 0.5 )

        # can also take list as argument
        # code_parameters.set_parameter( 'parameters/multiplication_factor', [0.1, 0.2, 0.3] )
        # <multiplication_factor>0.1 0.2 0.3</multiplication_factor>

        # updates (and validates) parameters
        actor_object.initialize(code_parameters=code_parameters)

.. _actor_settings_anchor:

Actor runtime settings
######################################################################################################################

The ``runtime_settings`` property tells the actor how the code should be called and defines:

-   Run mode
-   Debug settings
-   MPI settings
-   Batch settings
-   Temporary IDS storage settings
-   Command line to be run

*runtime_settings* cannot be accessed directly, but only via a special getter method:

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

-   Defined by setting one of predefined values

-   ``NORMAL`` (default) - the code is loaded as a library and its routines are called directly from Python,
    within the same process (and environment) used for the workflow script. Usually system resources,
    shared with other Python threads are limited, however this mode is suitable for most of the actors.

-   ``STANDALONE``   - the actor runs the code as an executable in a separate process, having its
    own environment and (usually) bigger system resources available. This mode is set automatically for MPI
    applications, however it can be set automatically e.g. for memory demanding code. It  has also its limitations:
    only `INIT`, `MAIN` and `FINALIZE` methods of the code can be run in the 'STANDALONE mode.

-   ``BATCH`` - an actor standalone executable is submitted to a batch queue.
    See `Batch settings`_ for details concerning batch job configuration

-  Example of the usage:

   .. code-block:: Python

       ...
       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       # configures runtime settings
       runtime_settings.run_mode = "STANDALONE"

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)

Debug settings
=========================================================================================

Debug mode
-----------------------------------------------------

-   Defined by setting one of predefined values

-   ``STANDALONE``   - similarly to STANDALONE *run mode* - an actor runs *the code as an executable
    in a separate process*, but this time under debugger control. Debugged code can be run several
    times. To proceed with workflow execution is enough to close the debugger. This debugging mode is suitable
    for most of the purposes.

-   ``ATTACH``   - an actor runs a debugger as parallel process, attaching it to a running workflow
    and setting breakpoint on wrapped code of the debugged actor.  Because debugger attaches to a
    workflow (and not a particular actor) killing debugged process kills the whole workflow. This mode has to be
    chosen if the issue within the code cannot be reproduced in STANDALONE mode and the issue results from actor
    interdependencies (e.g. one actor overwrites memory of the other one).

-   Example of the usage:

    .. code-block:: Python

       ...
      # gets runtime settings
      runtime_settings = actor_object.get_runtime_settings()

      #configures runtime settings
      runtime_settings.debug_mode = 'STANDALONE'
      # OR
      runtime_settings.debug_mode = DebugMode.STANDALONE

      # updates runtime_settings
      actor_object.initialize(runtime_settings=runtime_settings)


Setting debugger
-----------------------------------------------------
More advanced users may use a debbuger different than the default one. This can be controlled through the actor API

-   Defined by setting ``debugger_cmd`` or ``debugger_attach_cmd`` attributes

-   ``debugger_cmd``

    - Used only if debug mode is set to ``STANDALONE``
    - Attribute replaces default debugger
    - It should be set to debugger executable (e.g. 'gdb', 'totalview', etc)
    - Example of the usage:

      .. code-block:: Python

       ...
       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       # configures runtime settings
       runtime_settings.debug_mode = "STANDALONE"
       runtime_settings.debugger.debugger_cmd = 'gdb'

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)


-   ``debugger_attach_cmd``

    - Used only if debug mode is set to ``ATTACH``
    - Attribute replaces default command, which runs separate process that attaches to Python process
    - The syntax of command is usually a bit comples (see example below)
    - Example of the usage:

      .. code-block:: Python

           ...
           # gets runtime settings
           runtime_settings = actor_object.get_runtime_settings()

           # configures runtime settings
           runtime_settings.debug_mode = "ATTACH"
           runtime_settings.debugger.debugger_attach_cmd = "xterm -e gdb  -ex 'set breakpoint pending on'  -ex 'attach ${process_id}' -ex 'break ${main_sbrt_name}' -ex 'continue'"

           # updates runtime_settings
           actor_object.initialize(runtime_settings=runtime_settings)

MPI settings
=========================================================================================

-   *mpi_processes* - number of MPI processes to be used (default 1)

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
       runtime_settings.mpi.mpi_processes = 4
       runtime_settings.mpi.mpi_runner = 'mpirun'
       runtime_settings.mpi.mpi_options = '-tv'

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)

.. note::
   -  MPI code is *always* run as executable in standalone or batch mode
   -  If the code is not marked as 'MPI' (i.e. MPI compiler is not set) during actor generation,
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

-   *mode* - defines how sandbox is managed. One of the predefined values:

    -   ``MANUAL`` - full manual mode. It is developer responsibility to maintain sandbox
        (i.e. create it, clean it up, etc), Requires *path* attribute to be set.

    -   ``AUTOMATIC`` - iWrap generated actor manages the sandbox creation, clean up, etc

-   *path* - a **valid** path to an existing directory, that in 'manual' mode will be used as a sandbox.
    In 'automatic' mode directory is created by an actor...

-   *life_time* - defines when the sandbox will be cleaned up and removed. One of the predefined values
    of the class SandboxLifeTime or corresponding string:

    -   ``ACTOR_RUN`` - content of the sandbox directory is cleaned before and after
        every main actor method execution.

    -   ``WORKFLOW_RUN`` - content of the sandbox directory is cleaned, during initialising stage
        of an actor and after other finalisation actions of the actor (so, sandbox should be available during
        the whole workflow run)

    -   ``PERSISTENT`` - content of the sandbox directory is preserved and never cleaned up

-   Import of enumerated values:

    .. code-block:: Python

      from <actor name>.common.runtime_settings import SandboxLifeTime, SandboxMode

-   Example of the usage:

   .. code-block:: Python

       from <actor name>.common.runtime_settings import SandboxMode

       ...
       # gets runtime settings
       runtime_settings = actor_object.get_runtime_settings()

       # configures runtime settings
       runtime_settings.sandbox.mode = "MANUAL"
       # OR
       runtime_settings.sandbox.mode = SandboxMode.MANUAL

       runtime_settings.sandbox.path = '/path/to/existing/sandbox/directory'

       # updates runtime_settings
       actor_object.initialize(runtime_settings=runtime_settings)


IDS storage settings
=========================================================================================

This attribute defines settings of temporary storage being used while passing IDSes between the actor and the code.

-  Storage parameters that can be set:

   -   db_name:

       -  Meaning: name of data base to be used
       -  Default value: 'tmp'

   -   backend:

       -  Meaning - backend to be used
       -  Default value -   ``imas.imasdef.MEMORY_BACKEND``

   -    persistent_backend

        -  Meaning - backend to be used when temporary data cannot be stored in memory (e.g. while running
           the actor in standalone mode, when the code is run in a separate process, so it doesn't share
           memory with the workflow).
        -  Default value -  ``imas.imasdef.MDSPLUS_BACKEND``

.. note::
   Please note: for most of the purposes it is fine to not set this property and leave default values unchanged.

User defined commandline command
=========================================================================================
The workflow developer may take full control on the way an actor is run by defining the ``commandline_cmd``
attribute of ``runtime_settings``. If not set, the automatically generated commandline will be used
(typical for the most of the usage scenarios)


.. _command_tag_usage_anchor:
.. note::
   User defined commandline may contain predefined tags (syntax ``${tag}``), where ``tag``
   is the name of one of the runtime_settings attributes. The usage of tags is optional.
   The only exception is ``${exec}`` tag (representing full path to executable binary),
   which MUST be present in the commandline.

Actor information
######################################################################################################################

.. _actor_and_code_descriptions_anchor:

Actor and code descriptions
=========================================================================================

Actor and code descriptions can be obtained using generated actor's ``actor_description`` and ``code_description`` attributes.
Both attributes are type of dictionary and store information defined in yaml file used for actor generation.

For list of available actor description information see: :ref:`yaml_actor_description_anchor`

For list of available code description information see: :ref:`yaml_code_description_anchor`

``actor_description`` attribute example value:

.. code-block:: python

    from physics_ii.actor import physics_ii

    actor_physics_ii = physics_ii()
    print(actor_physics_ii.actor_description)
    >>>
    {'actor_name': 'physics_ii',
     'actor_type': '',
     'data_type': '',
     'install_dir': ''}

``code_description`` attribute example value:

.. code-block:: python

    from physics_ii.actor import physics_ii

    actor_physics_ii = physics_ii()
    print(actor_physics_ii.code_description)
    >>>
    {'arguments':
     [
      {'intent': 'IN', 'name': 'equilibrium_in', 'type': 'equilibrium'},
      {'intent': 'OUT', 'name': 'equilibrium_out', 'type': 'equilibrium'}
     ],
     'documentation': 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. ',
      'implementation':
       {'code_parameters':
        {'parameters': '/gss_efgw_work/work/username/iwrap/examples/level2/input/input_physics.xml',
         'schema': '/gss_efgw_work/work/username/iwrap/examples/level2/input/input_physics.xsd'
        },
        'code_path': '/gss_efgw_work/work/username/iwrap/examples/level2/native_code/libphysics_ii.a',
        'data_dictionary_compliant': '3.37.0',
        'data_type': 'legacy',
        'include_path': '/gss_efgw_work/work/username/iwrap/examples/level2/native_code/mod_physics_ii.mod',
        'programming_language': 'fortran',
        'root_dir': '.',
        'subroutines':
        {'finalize': '',
         'get_state': '',
         'get_timestamp': '',
         'init': '',
         'main': 'physics_ii',
         'set_state': ''
        }
       },
       'settings':
       {'compiler_cmd': 'gfortran',
        'extra_libraries':
         {'path_defined': [],
          'pkg_config_defined': ['xmllib']
         },
         'mpi_compiler_cmd': None,
         'compiler_flags': None
       }
    }



.. _actor_build_info_anchor:

Actor build info
=========================================================================================



Actor build info can be accessed through actor instance's ``build_info`` attribute.
All fields are filled automatically during actor build process and cannot be changed.


Build info contains:
    - iWrap version - version of iWrap used to generate accessed actor eg. ``0.6.0``
    - IMAS (DD) version - version of IMAS loaded during actor generation eg. ``3.37.0``
    - IMAS prefix - full prefix of IMAS eg. ``/gw/swimas/core/IMAS/3.37.0/AL/4.11.0/gcc/7.3.0``
    - AL version - version of Access Layer loaded during actor generation eg. ``4.11.0``
    - Generation date - full datetime of actor generation eg. ``2023-01-01 06:12:24``

Workflow developer may access actor build info as follows:

.. code-block:: python

    from physics_ii.actor import physics_ii

    actor_physics_ii = physics_ii()
    build_info_dict  = actor_physics_ii.build_info

    iwrap_version    = build_info_dict.get("iwrap_version")
    imas_version     = build_info_dict.get("imas_version")
    imas_prefix      = build_info_dict.get("imas_prefix")
    al_version       = build_info_dict.get("al_version")
    generation_date  = build_info_dict.get("generation_date")


Logging
######################################################################################################################

The ``logging_config`` method configures logging of information on various levels and allows users to decide which level of logging to use for each actor individually.

It takes the following arguments:

- ``level`` (str | int): Logging severity below which messages are not logged. May be passed as one of the following: a string (case-insensitive) or an integer that correspond to the ``logging`` library level, or a ``logging`` library constant (see table below). Required argument.

========  =======  ================
string    integer  constant
========  =======  ================
debug     10       logging.DEBUG
info      20       logging.INFO
warning   30       logging.WARNING
error     40       logging.ERROR
critical  50       logging.CRITICAL
========  =======  ================

- ``stream``: Defines a stream to which logging output will be sent: it could be to streams such as sys.stdout, sys.stderr or any file-like object (any object which supports write() method). Optional, defaults to stderr.

``logging_config`` method returns ``logging.Logger``: a configured logger with a name of an actor.

Usage:

.. code-block:: python

        log_file = open('actor.log', 'a')
        logger = self.actor.logging_config('info', log_file)
        logger.info('test logging')
        logger.warning('some warning')

For a usage example see ``examples/dummy_actor``.


The workflow example
######################################################################################################################

.. _workflow_example_anchor:

.. code-block:: python

    import sys
    import imas,os

    from physics_ii.actor import physics_ii



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

            runtime_settings.debug_mode = "STANDALONE"
            runtime_settings.sandbox.life_time  = "PERSISTENT"

            code_parameters = self.actor_physics_ii.get_code_parameters()
            value = code_parameters.get_parameter('parameters/multiplication_factor')
            code_parameters.set_parameter( 'parameters/multiplication_factor', 0.5 )
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


