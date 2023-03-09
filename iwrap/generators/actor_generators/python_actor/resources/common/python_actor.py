import os
import logging
import sys


from .code_parameters import CodeParameters

from .runtime_settings import RuntimeSettings, SandboxLifeTime, DebugMode, RunMode
from .base_actor import ActorBaseClass
from ..binding.binder import CBinder
from .runners import Runner


class PythonActor(ActorBaseClass):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self):
        super().__init__()
        self.__binder = CBinder()
        self.__runner = None

    def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None):
        """
        Attributes:
            runtime_settings (RuntimeSettings): Path to a XML file with code parameters
            code_parameters (CodeParameters): Path to a XSD file with schema definition for code parameters file
        """
        super().initialize(runtime_settings, code_parameters)
        self.__binder.initialize(actor=self)

        Runner.initialize(self, self.__binder, self.sandbox.path, self.output_stream)

        self.__runner = Runner.get_runner(self.is_standalone)

        code_parameters_str = None
        if code_parameters:
            code_parameters_str = code_parameters.parameters

        self.sandbox.jump_in()
        self.__runner.call_initialize( code_parameters=code_parameters_str )
        self.sandbox.jump_out()

    def run(self, *args):

        code_parameters_str = None
        code_parameters = self.get_code_parameters()
        if code_parameters:
            code_parameters_str = code_parameters.parameters

        self.sandbox.jump_in()
        out = self.__runner.call_main( *args , code_parameters=code_parameters_str)
        self.sandbox.jump_out()

        self.sandbox.clean()

        return out

    def finalize(self):
        self.sandbox.jump_in()
        self.__runner.call_finalize()
        self.sandbox.jump_out()

        self.__binder.finalize()
        self.sandbox.remove()

    def get_state(self) -> str:
        self.sandbox.jump_in()
        state = self.__runner.call_get_state()
        self.sandbox.jump_out()
        return state

    def set_state(self, state: str) -> None:
        self.sandbox.jump_in()
        self.__runner.call_set_state(state)
        self.sandbox.jump_out()

    def get_timestamp(self) -> float:
        self.sandbox.jump_in()
        timestamp = self.__runner.call_get_timestamp()
        self.sandbox.jump_out()
        return timestamp