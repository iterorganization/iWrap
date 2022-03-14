import logging
from enum import Enum, auto

import imas

DEBUGGER_TOTALVIEW = 10
DEBUGGER_GDB = 20


class DebugMode( Enum ):
    NONE = auto()
    STANDALONE = auto()
    ATTACH = auto()


class RunMode( Enum ):
    NORMAL = auto()
    STANDALONE = auto()
    BATCH = auto()


class SandboxMode( Enum ):
    AUTOMATIC = auto()
    MANUAL = auto()

class SandboxLifeTime( Enum ):
    ACTOR_RUN = auto()
    WORKFLOW_RUN = auto()
    PERSISTENT = auto()


class IdsStorageSettings:
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        self.db_name = 'tmp'
        self.backend = imas.imasdef.MEMORY_BACKEND
        self.persistent_backend = imas.imasdef.MDSPLUS_BACKEND


class RuntimeSettings:

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

        @property
        def batch_default_runner(self):
            return self._default_runner

        @property
        def batch_default_options(self):
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

        @property
        def mpi_default_runner(self):
            return self._default_runner

        @property
        def mpi_default_options(self):
            return self._default_options


        @property
        def mpi_nodes(self):
            raise AttributeError( '"mpi_nodes" attribute has been removed please use "mpi_processes" instead')

        @mpi_nodes.setter
        def mpi_nodes(self, value):
            raise AttributeError( '"mpi_nodes" attribute has been removed please use "mpi_processes" instead')


        def __init__(self):
            self.mpi_processes = 1
            self.mpi_runner = None
            self._default_runner = None
            self._default_options = None
            self.mpi_options = None


    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class OpenMPSettings:
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class SandboxSettings:
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.path: str = ''
            self.life_time: SandboxLifeTime = SandboxLifeTime.ACTOR_RUN
            self.mode: SandboxMode = SandboxMode.AUTOMATIC
