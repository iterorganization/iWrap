import logging
from enum import Enum, auto

import imas


class DebugMode( Enum ):
    """Provides enumerated values describing run mode
    """

    NONE = auto()
    """Debug turned off"""

    STANDALONE = auto()
    """An actor runs native code as executable in a separate system process, under debugger control. Debugged code 
    can be run several times. To proceed with workflow execution is enough to close the debugger. 
    This debugging mode is suitable for most of the purposes."""

    ATTACH = auto()
    """An actor runs a debugger as parallel process, attaching it to a running workflow and setting breakpoint 
    on wrapped native code of the debugged actor. Because debugger attaches to a workflow (and not a particular actor) 
    killing debugged process kills the whole workflow. 
    This mode has to be chosen if the issue within code cannot be reproduced in STANDALONE mode and the issue results 
    from actor interdependencies (e.g. one actor overwrites memory of the other one)."""

class RunMode( Enum ):
    """Provides enumerated values describing run mode
    """

    NORMAL = auto()
    """Native code is loaded from system library and called directly from Python, 
    within the same process (and environment) that workflow script. Usually system resources, shared with other Python 
    threads are limited, however this mode is suitable for most of the actors."""

    STANDALONE = auto()
    """An actor runs native code as executable in a separate system process, having its own environment 
    and (usually) bigger system resources available. This mode is set automatically for MPI applications,
    however it can be set also e.g. for memory demanding code."""

    BATCH = auto()
    """An actor standalone executable is submitted to a batch queue."""


class SandboxMode( Enum ):
    """Provides enumerated values describing sandbox mode
    """

    AUTOMATIC = auto()
    """iWrap generated actor manages the sandbox creation, clean up, etc"""

    MANUAL = auto()
    """Full manual mode. It is developer responsibility to maintain sandbox (i.e. create it, clean it up, etc).
     Requires :obj:`SandboxSettings` path attribute to be set."""


class SandboxLifeTime( Enum ):
    """Provides enumerated values describing life time of the sandbox
    """

    ACTOR_RUN = auto()
    """Content of the sandbox directory is cleaned before and after every main actor method execution"""

    WORKFLOW_RUN = auto()
    """Content of the sandbox directory is cleaned, during initialising stage of an actor and after 
    other finalisation actions of the actor (so, sandbox should be available during the whole workflow run)"""

    PERSISTENT = auto()
    """Content of the sandbox directory is preserved and never cleaned up"""

class IdsStorageSettings:
    """Settings of temporary storage being used while passing IDSes between an actor and native code.

    Attributes:
        db_name (str, default='tmp'): name of the data base to be used
        backend (str, default=imas.imasdef.MEMORY_BACKEND): backend to be used
        persistent_backend  (str, default=imas.imasdef.MDSPLUS_BACKEND): backend to be used when temporary data
            cannot be stored in memory (e.g. while running actor in a standalone mode,
            when a native code is run as separate process, so it doesn’t share memory with other actors.
    """

    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        self.db_name = 'tmp'
        self.backend = imas.imasdef.MEMORY_BACKEND
        self.persistent_backend = imas.imasdef.MDSPLUS_BACKEND


class RuntimeSettings:
    """The runtime settings determines how native code should be run.

   Attributes:
        run_mode (:obj:`RunMode`): Defined by setting one of predefined :obj:`RunMode` enumeration class values
        debug_mode (:obj:`DebugMode`, default=DebugMode.NONE): debugging mode
        ids_storage (:obj:`IdsStorageSettings`): temporary data cache settings
        mpi (:obj:`MPISettings`): MPI job settings
        sandbox (:obj:`SandboxSettings`): sandbox settings
        batch (:obj:`BatchSettings`): batch job settings
        debugger (:obj:`DebuggerSettings`): debugger settings
        commandline_cmd (`str`): user provided commandline string that replaces automatically generated one
        exec_options(`str`): additional user options to be added to automatically generated commandline

    """

    def __init__(self):
        # handled/implemented
        self.run_mode = RunMode.NORMAL
        self.debug_mode = DebugMode.NONE
        self.ids_storage = IdsStorageSettings()
        self.commandline_cmd = ''
        self.exec_options = ''
        self.mpi = None
        self.sandbox = self.SandboxSettings()
        self.batch = self.BatchSettings()
        self.debugger = self.DebuggerSettings()
        # not implemented yet

        self.open_mp = None

        self.TBD = None  # any other info needed?

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class BatchSettings:
        """The class keeps information of batch execution settings

        Attributes:
            batch_nodes (`int`, default = 1): number of nodes to be used (default 1)
            batch_runner (`str`, default = 1): user defined batch runner to be used. If not provided batch_default_runner is used
            batch queue (`str`): batch queue to be used
            batch_options (`str`): user defined options for batch runner
        """
        @property
        def batch_default_runner(self):
            """Default batch runner (sbatch, srun, etc) to be used. Its value is platform dependent, read from iWrap configuration. Read only attribute
            Returns:
                `str`: default batch runner
            """
            return self._default_runner

        @property
        def batch_default_options(self):
            """Default batch runner (sbatch, srun, etc) to be used. Its value is platform dependent, read from iWrap configuration. Read only attribute
            Returns:
              `str`: default batch options
            """
            return self._default_options


        def __init__(self):
            self._default_runner = ''
            self.batch_runner = ''
            self.batch_queue = ''
            self.batch_nodes = 1
            self._default_options = ''
            self.batch_options = ''

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class DebuggerSettings:
        """ Keeps information about command needed to run debugging session

        """

        @property
        def debugger_default_cmd(self):
            return self._default_cmd

        @property
        def debugger_default_attach_cmd(self):
            return self._default_attach_cmd

        def __init__(self):
            self._default_cmd = ''  # TotalView/gdb
            self.debugger_cmd = ''
            self._default_attach_cmd = ''
            self.debugger_attach_cmd = ''

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class MPISettings:
        """The class keeps information of MPI execution

        Attributes:
            mpi_processes (`int`, default = 1): number of MPI nodes to be used (default 1)
            mpi_runner (`str`): user defined MPI runner to be used. If not provided mpi_default_runner is being used
            mpi_options (`str`):  user defined options for MPI runner (e.g. -tv or --debug for debugging).
        """

        @property
        def mpi_default_runner(self) -> str:
            """default MPI runner (mpiexec, mpirun, etc) to be used. Its value is platform dependent, read from iWrap configuration. Read only attribute
            Returns:
                `str`: default MPI runner
            """
            return self._default_runner

        @property
        def mpi_default_options(self) -> str:
            """default MPI runner options to be added to command line. Its value is platform dependent, read from iWrap configuration. Read only attribute

            Returns:
                `str`: default MPI options
            """
            return self._default_options


        @property
        def mpi_nodes(self):
            raise AttributeError( '"mpi_nodes" attribute has been removed please use "mpi_processes" instead')

        @mpi_nodes.setter
        def mpi_nodes(self, value):
            raise AttributeError( '"mpi_nodes" attribute has been removed please use "mpi_processes" instead')

        def __init__(self):
            """

            """
            self.mpi_processes: int = 1
            self.mpi_runner: str = None
            self._default_runner: str = None
            self._default_options: str=None
            self.mpi_options: str = None


    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class OpenMPSettings:
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class SandboxSettings:
        """'Sandbox' - a directory, in which actor will be run.

        Before execution of user codes wrapped by an iWrap generated actor,
        directory will be changed to “sandbox”, and after actor finishes,
        current directory will be switched back to previous value.
        The name (path) of “sandbox” directory will be created automatically or has to be specified by user.

        Attributes:
            mode (:obj:`SandboxMode`, default = SandboxMode.AUTOMATIC): defines how sandbox is managed.
                One of the predefined values of class SandboxMode:
            path (str): a valid path to an existing directory, that in ‘manual’ mode will be used as a sandbox.
                In ‘automatic’ mode directory is created by an actor…
            life_time (:obj:`SandboxLifeTime`, default=SandboxLifeTime.ACTOR_RUN): defines when the sandbox
                will be cleaned up and removed. One of the predefined values of the class :obj:`SandboxLifeTime`
        """
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.path: str = ''
            self.life_time: SandboxLifeTime = SandboxLifeTime.ACTOR_RUN
            self.mode: SandboxMode = SandboxMode.AUTOMATIC
