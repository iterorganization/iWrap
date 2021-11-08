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


class SandboxMode( Enum ):
    AUTOMATIC = auto()
    MANUAL = auto()

class SandboxLifeTime( Enum ):
    ACTOR_RUN = auto()
    WORKFLOW_RUN = auto()
    PERSISTENT = auto()


class IdsStorage:
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        self.db_name = 'tmp'
        self.shot = 9999
        self.run = 9999
        self.backend = imas.imasdef.MEMORY_BACKEND
        self.persistent_backend = imas.imasdef.MDSPLUS_BACKEND


class RuntimeSettings:
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        # handled/implemented
        self.run_mode = RunMode.NORMAL
        self.debug_mode = DebugMode.NONE
        self.ids_storage = IdsStorage()
        self.mpi = self.MPI()
        self.sandbox = self.SandboxSettings()
        self.batch = self.Batch()
        # not implemented yet

        self.open_mp = self.OpenMP()

        self.TBD = None  # any other info needed?

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class BatchJob:
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.runner = None
            self.queue = None
            self.default_options = None
            self.options = None


    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class Debug:
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.debugger = None  # TotalView/gdb
            self.mode = None  # attach/standalone
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class MPI:
        # Class logger
        __logger = logging.getLogger( __name__ + "." + __qualname__ )

        def __init__(self):
            self.number_of_processes = 1
            self.runner = None
            self.default_options = None
            self.options = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class OpenMP:
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
