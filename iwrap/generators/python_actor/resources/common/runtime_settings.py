import logging
from enum import Enum, auto

import imas

DEBUGGER_TOTALVIEW = 10
DEBUGGER_GDB = 20


class DebugMode(Enum):
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    NONE = auto()
    STANDALONE = auto()
    ATTACH = auto()

class RunMode(Enum):
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    NORMAL = auto()
    STANDALONE = auto()


class IdsStorage:
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):
        self.db_name = 'tmp'
        self.shot = 9999
        self.run = 9999
        self.backend = imas.imasdef.MEMORY_BACKEND
        self.persistent_backend = imas.imasdef.MDSPLUS_BACKEND


class SandboxSettings:
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    LIFETIME_ACTOR = 10
    LIFETIME_SCENARIO = 20


class RuntimeSettings:
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):

        # handled/implemented
        self.run_mode = RunMode.NORMAL
        self.debug_mode = DebugMode.NONE
        self.ids_storage = IdsStorage()
        self.mpi = self.MPI()

        # not implemented yet
        self.batch_job = self.BatchJob()
        self.open_mp = self.OpenMP()
        self.sandbox = self.Sandbox()
        self.TBD = None  # any other info needed?

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class BatchJob:
        # Class logger
        logger = logging.getLogger(__name__ + "." + __qualname__)

        def __init__(self):
            self.queue = None
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class Debug:
        # Class logger
        logger = logging.getLogger(__name__ + "." + __qualname__)

        def __init__(self):
            self.debugger = None  # TotalView/gdb
            self.mode = None  # attach/standalone
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class MPI:
        # Class logger
        logger = logging.getLogger(__name__ + "." + __qualname__)

        def __init__(self):
            self.number_of_processes = 1
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class OpenMP:
        # Class logger
        logger = logging.getLogger(__name__ + "." + __qualname__)

        def __init__(self):
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class Sandbox:
        # Class logger
        logger = logging.getLogger(__name__ + "." + __qualname__)

        def __init__(self):
            self.path = None
            self.lifetime = None
            self.TBD = None