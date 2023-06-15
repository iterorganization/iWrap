from abc import ABC, abstractmethod

import os
import logging
import string
from threading import Thread


from .runtime_settings import DebugMode
from . import cmdln_utlis
from . import exec_system_cmd


class Runner(ABC):
    __standalone_runner = None
    __library_runner = None


    @classmethod
    def get_runner(cls, actor):

        if not cls.__library_runner or not cls.__standalone_runner:
            cls.initialize(actor)

        if actor.is_standalone:
            return cls.__standalone_runner

        return cls.__library_runner

    @classmethod
    def initialize(cls, actor):
        cls.__standalone_runner = StandaloneRunner(actor)
        cls.__library_runner = LibraryRunner(actor)

    def __init__(self, actor):
        from ..binding.binder import CBinder
        self._actor = actor
        self._binder = CBinder()
        self._output_stream = actor.output_stream
        self._runtime_settings = actor.get_runtime_settings()
        self._binder.initialize(actor=actor)

    def get_code_parameters(self):
        code_parameters = self._actor.get_code_parameters()
        if not code_parameters:
            return None

        return code_parameters.parameters

    @abstractmethod
    def call_initialize(self):
        ...

    @abstractmethod
    def call_main(self, *ids_list):
        ...

    @abstractmethod
    def call_finalize(self):
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

    def call_initialize(self):
        code_parameters = self.get_code_parameters()
        output = self.__run_standalone(method_name="init", arg_metadata_list = [],
                                       code_parameters=code_parameters)
        return output

    def call_main(self, *ids_list):
        code_parameters = self.get_code_parameters()
        arg_metadata_list = self._actor.code_description.get('arguments')
        output = self.__run_standalone(*ids_list, method_name="main", arg_metadata_list = arg_metadata_list,
                                       code_parameters=code_parameters)
        return output

    def call_finalize(self):
        output = self.__run_standalone(method_name="finalize", arg_metadata_list=[],
                                       code_parameters=None)
        return output

    def __run_standalone(self, *ids_list, method_name,  arg_metadata_list, code_parameters = None):

        self.__logger.debug( "RUNNING STDL" )


        runtime_settings = self._runtime_settings

        if runtime_settings.commandline_cmd:
            exec_command:str = runtime_settings.commandline_cmd
            cmdln_utlis.validate_command(exec_command)
        else:
            exec_command = cmdln_utlis.create_cmd(runtime_settings)

        exec_command = cmdln_utlis.resolve_cmd_tags( self._actor,  method_name, exec_command, runtime_settings )

        print( f'COMMAND: {exec_command}' )
        # debug_mode:            exec_command.append( 'totalview' )

        sandbox_dir = self._actor.sandbox.path
        results = self._binder.run_standalone( *ids_list,
                                               method_name=method_name,
                                               arg_metadata_list=arg_metadata_list,
                                               code_parameters=code_parameters,
                                               exec_command = exec_command,
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

    def __attach_debugger(self):
        actor_name = self._actor.name

        debugger = self._runtime_settings.debugger
        debugger_attach_cmd = None
        if debugger.debugger_attach_cmd:
            debugger_attach_cmd =  debugger.debugger_attach_cmd
        else:
            debugger_attach_cmd = debugger.debugger_default_attach_cmd

        process_id = os.getpid()

        debugger_attach_cmd = string.Template( debugger_attach_cmd ).substitute( process_id=f'{process_id}',
                                                                                 init_sbrt_name=f'init_{actor_name}_wrapper',
                                                                                 main_sbrt_name=f'{actor_name}_wrapper',
                                                                                 finish_sbrt_name=f'finish_{actor_name}_wrapper',
                                                                                 set_state_sbrt_name = f'set_state_{actor_name}_wrapper',
                                                                                 get_state_sbrt_name=f'get_state_{actor_name}_wrapper',
                                                                                 get_timestamp_sbrt_name=f'get_timestamp_{actor_name}_wrapper'

        )

        def start_debugger(debugger_attach_cmd):
            self.__logger.debug( 'EXECUTING command: ' + str( debugger_attach_cmd ) )
            exec_system_cmd( debugger_attach_cmd, output_stream=self._actor.output_stream )

        t = Thread( target=start_debugger, args=(debugger_attach_cmd,) )
        t.daemon = True  # thread dies with the program
        t.start()
        input()  # just to wait until debugger starts

    def call_initialize(self):

        if self._runtime_settings.debug_mode != DebugMode.NONE:
            self.__attach_debugger()

        code_parameters = self.get_code_parameters()
        output = self._binder.call_init(code_parameters=code_parameters)
        return output

    def call_main(self, *ids_list):

        self.__logger.debug( "RUNNING Lib" )
        code_parameters = self.get_code_parameters()
        results = self._binder.call_main(*ids_list, code_parameters=code_parameters)
        return results

    def call_finalize(self):
        self._binder.call_finish()
        self._binder.finalize()

    def call_get_state(self) -> str:
        state = self._binder.call_get_state()
        return state

    def call_set_state(self, state: str):
        self._binder.call_set_state(state)

    def call_get_timestamp(self) -> float:
        timestamp = self._binder.call_get_timestamp()
        return timestamp
