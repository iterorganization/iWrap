import os
import logging
import sys
from abc import ABC, abstractmethod
from typing import Union, List, Any

from .code_parameters import CodeParameters

from .runtime_settings import RuntimeSettings, SandboxLifeTime, DebugMode, RunMode
from ..binding.binder import CBinder
from .sandbox import Sandbox
from .runners import Runner


class OutputStatus:
    def __init__(self):
        self.code: int = 0
        self.message: str = None


class Actor(ABC):

    @abstractmethod
    def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:
        ...

    @abstractmethod
    def run(self, *args) -> Union[List[Any], Any]:
        ...

    @abstractmethod
    def finalize(self) -> None:
        ...

class ActorBaseClass(Actor):
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

    def get_runtime_settings(self) -> RuntimeSettings:
        return self.__runtime_settings

    def get_code_parameters(self) -> CodeParameters:
        return self.__code_parameters

    def __init__(self):
        self.output_stream = sys.stdout
        self.is_mpi_code = False
        self.__runtime_settings: RuntimeSettings = RuntimeSettings()
        self.sandbox: Sandbox = None
        self.arguments = []
        self.__code_parameters: CodeParameters = None

        self.name = self.__class__.__name__

        self.__binder = CBinder()
        self.__runner = None

    def is_standalone_run(self):
        if self.is_mpi_code:
            return True

        if self.__runtime_settings.debug_mode == DebugMode.STANDALONE:
            return True

        if self.__runtime_settings.run_mode == RunMode.STANDALONE:
            return True

        if self.__runtime_settings.run_mode == RunMode.BATCH:
            return True

        return False

    # # #  Actor lifecycle methods # # #

    def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None):
        """
        Attributes:
            runtime_settings (RuntimeSettings): Path to a XML file with code parameters
            code_parameters (CodeParameters): Path to a XSD file with schema definition for code parameters file
        """

        code_parameters_str = None
        if self.__code_parameters:
            if code_parameters:
                self.__code_parameters = code_parameters

            self.__code_parameters.initialize()
            code_parameters_str = self.__code_parameters.parameters

        if runtime_settings:
            self.__runtime_settings = runtime_settings

        self.sandbox = Sandbox(self)

        self.sandbox.initialize()
        self.__binder.initialize(actor=self)


        Runner.initialize(self, self.__binder, self.sandbox.path, self.output_stream)
        is_standalone = self.is_standalone_run()
        self.__runner = Runner.get_runner(is_standalone)
        self.__runner.call_initialize( code_parameters_str )

    def __call__(self, *args):
        return self.run( *args )

    def run(self, *args):

        code_parameters = None
        if self.__code_parameters:
            code_parameters = self.__code_parameters.parameters

        out = self.__runner.call_main( *args , code_parameters=code_parameters)

        if self.__runtime_settings.sandbox.life_time == SandboxLifeTime.ACTOR_RUN:
            self.sandbox.clean()

        return out

    def finalize(self):

        self.__runner.call_finalize()

        self.__binder.finalize()
        self.sandbox.remove()

