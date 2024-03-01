from abc import ABC, abstractmethod

import os
import logging
import string
from threading import Thread

from .runtime_settings import DebugMode
from . import cmdln_utlis
from . import exec_system_cmd


class Runner(ABC):

    @classmethod
    def get_runner(cls, actor):

        if actor.is_standalone:
            runner = StandaloneRunner(actor)
        else:
            runner = LibraryRunner(actor)

        runner.initialize()

        return runner

    def __init__(self, actor):
        from ..binding.binder import LanguageBinder
        self._actor = actor
        self._binder = LanguageBinder()
        self._output_stream = actor.output_stream
        self._runtime_settings = actor.get_runtime_settings()

    def initialize(self):
        self._binder.initialize(actor=self._actor)

    def finalize(self):
        self._binder.finalize()

    def get_code_parameters(self):
        code_parameters = self._actor.get_code_parameters()
        if not code_parameters:
            return None

        return code_parameters.parameters

    @abstractmethod
    def call_initialize(self, *ids_list):
        ...

    @abstractmethod
    def call_main(self, *ids_list):
        ...

    @abstractmethod
    def call_finalize(self, *ids_list):
        ...

    @abstractmethod
    def call_get_state(self) -> str:
        ...

    @abstractmethod
    def call_set_state(self, state: str):
        ...

    def call_get_timestamp(self) -> float:
        ...

class StandaloneRunner( Runner ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self, actor):
        super().__init__( actor)

    def call_initialize(self,  *ids_list):
        code_parameters = self.get_code_parameters()
        output = self.__run_standalone(*ids_list, method_name="init",
                                       code_parameters=code_parameters)
        return output

    def call_main(self, *ids_list):
        code_parameters = self.get_code_parameters()
        output = self.__run_standalone(*ids_list, method_name="main",
                                       code_parameters=code_parameters)
        return output

    def call_finalize(self,  *ids_list):
        output = self.__run_standalone(*ids_list, method_name="finalize",
                                       code_parameters=None)
        return output

    def __run_standalone(self, *ids_list, method_name, code_parameters = None):

        self.__logger.debug( "RUNNING STDL" )

        arg_metadata_list = self._actor.code_description['implementation']['subroutines'][method_name].get('arguments')
        runtime_settings = self._runtime_settings

        if runtime_settings.commandline_cmd:
            full_command:str = runtime_settings.commandline_cmd
            cmdln_utlis.validate_command(full_command)
        else:
            full_command = cmdln_utlis.create_cmd(runtime_settings)

        exec_cmd = self._binder.standalone_cmd(method_name)
        full_command = cmdln_utlis.resolve_cmd_tags(full_command, exec_cmd, runtime_settings )

        print( f'COMMAND: {full_command}' )
        # debug_mode:            exec_command.append( 'totalview' )

        sandbox_dir = self._actor.sandbox.path
        results = self._binder.run_standalone( *ids_list,
                                               method_name=method_name,
                                               arg_metadata_list=arg_metadata_list,
                                               code_parameters=code_parameters,
                                               exec_command = full_command,
                                               sandbox_dir = sandbox_dir,
                                               output_stream=self._output_stream )
        return results

    def call_get_state(self) -> str:
        ...

    def call_set_state(self, state: str):
        ...

    def call_get_timestamp(self) -> float:
        ...


class LibraryRunner( Runner ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self, actor):
        super().__init__( actor)

    def initialize(self):
        super().initialize()

    def call_initialize(self, *ids_list):

        code_parameters = self.get_code_parameters()
        output = self._binder.call_init(*ids_list, code_parameters=code_parameters)
        return output

    def call_main(self, *ids_list):

        self.__logger.debug( "RUNNING Lib" )
        code_parameters = self.get_code_parameters()
        results = self._binder.call_main(*ids_list, code_parameters=code_parameters)
        return results

    def call_finalize(self, *ids_list):
        code_parameters = self.get_code_parameters()
        output = self._binder.call_finish(*ids_list,  code_parameters=code_parameters)
        self._binder.finalize()
        return output

    def call_get_state(self) -> str:
        state = self._binder.call_get_state()
        return state

    def call_set_state(self, state: str):
        self._binder.call_set_state(state)

    def call_get_timestamp(self) -> float:
        timestamp = self._binder.call_get_timestamp()
        return timestamp
