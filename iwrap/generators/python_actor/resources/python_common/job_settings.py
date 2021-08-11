from enum import Enum, auto

import imas

DEBUGGER_TOTALVIEW = 10
DEBUGGER_GDB = 20


class DebugMode(Enum):
    NONE = auto()
    STANDALONE = auto()
    ATTACH = auto()

class RunMode(Enum):
    NORMAL = auto()
    STANDALONE = auto()


class IdsCache:
    def __init__(self):
        self.db_name = 'tmp'
        self.shot = 9999
        self.run = 9999
        self.backend = imas.imasdef.MEMORY_BACKEND

class SandboxSettings:
    LIFETIME_ACTOR = 10
    LIFETIME_SCENARIO = 20


class JobSettings:
    def __init__(self):

        # handled/implemented
        self.run_mode = RunMode.NORMAL
        self.debug_mode = DebugMode.NONE
        self.ids_cache = IdsCache()

        # not implemented yet
        self.batch_job = self.BatchJob()
        self.mpi = self.MPI()
        self.open_mp = self.OpenMP()
        self.sandbox = self.Sandbox()
        self.TBD = None  # any other info needed?

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class BatchJob:
        def __init__(self):
            self.queue = None
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class Debug:
        def __init__(self):
            self.debugger = None  # TotalView/gdb
            self.mode = None  # attach/standalone
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class MPI:
        def __init__(self):
            self.TBD = None
            self.debug_switch = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class OpenMP:
        def __init__(self):
            self.TBD = None

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    class Sandbox:
        def __init__(self):
            self.path = None
            self.lifetime = None
            self.TBD = None