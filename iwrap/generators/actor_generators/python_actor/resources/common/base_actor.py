import os
import logging
import sys
from abc import ABC

from .runtime_settings import RuntimeSettings, SandboxLifeTime, DebugMode, RunMode
from ..binding.binder import CBinder
from .sandbox import Sandbox
from .runners import Runner


class OutputStatus:
    def __init__(self):
        self.code: int = 0
        self.message: str = None

class ActorBaseClass( ABC ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    __instance_index = 0

    @property
    def unique_id(self):
        class_name = self.__class__.__name__
        idx = self.__instance_index
        pid = os.getpid()
        uid = f'{class_name}_{idx}#{pid}'
        return uid

    def __new__(cls):
        new_object = object.__new__( cls )
        cls.__instance_index += 1
        return new_object

    def __init__(self):
        self.output_stream = sys.stdout
        self.runtime_settings: RuntimeSettings = RuntimeSettings()
        self.sandbox: Sandbox = None
        self.arguments = []
        self.code_parameters = None

        self.name = self.__class__.__name__

        self.__binder = CBinder( )
        self.__runner = None

    def is_standalone_run(self):
        if self.is_mpi_code:
            return True

        if self.runtime_settings.debug_mode == DebugMode.STANDALONE:
            return True

        if self.runtime_settings.run_mode == RunMode.STANDALONE:
            return True

        return False

    # # #  Actor lifecycle methods # # #

    def initialize(self):

        if self.code_parameters:
            self.code_parameters.initialize()

        self.sandbox = Sandbox(self)

        self.__binder.initialize(actor=self)
        self.sandbox.initialize()


        # XML Code Params
        code_parameters = None
        if self.code_parameters:
            code_parameters = self.code_parameters.parameters

        Runner.initialize(self, self.__binder, self.sandbox.path, self.output_stream)
        is_standalone = self.is_standalone_run()
        self.__runner = Runner.get_runner(is_standalone)
        self.__runner.call_initialize( code_parameters )

    def __call__(self, *args):
        return self.run( *args )

    def run(self, *args):

        code_parameters = None
        if self.code_parameters:
            code_parameters = self.code_parameters.parameters

        out = self.__runner.call_main( *args , code_parameters=code_parameters)

        if self.runtime_settings.sandbox.life_time == SandboxLifeTime.ACTOR_RUN:
            self.sandbox.clean()

        return out

    def finalize(self):

        self.__runner.call_finalize()

        self.sandbox.clean()
        self.__binder.finalize()
        self.sandbox.remove()

