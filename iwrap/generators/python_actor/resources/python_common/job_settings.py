from enum import Enum, auto

DEBUGGER_TOTALVIEW = 10
DEBUGGER_GDB = 20


class DebugMode(Enum):
    NONE = auto()
    STANDALONE = auto()
    ATTACH = auto()

class RunMode(Enum):
    NORMAL = auto()
    STANDALONE = auto()

class SandboxSettings:
    LIFETIME_ACTOR = 10
    LIFETIME_SCENARIO = 20


class JobSettings:
    def __init__(self):

        # handled/implemented
        self.run_mode = RunMode.NORMAL
        self.debug_mode = DebugMode.NONE

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