from abc import ABC, abstractmethod

import os
import logging
import string
from threading import Thread

from .runtime_settings import RuntimeSettings, RunMode, DebugMode
from . import exec_system_cmd


class Runner(ABC):
    __standalone_runner = None
    __library_runner = None


    @classmethod
    def get_runner(cls, is_standalone: bool):
        if is_standalone:
            return cls.__standalone_runner

        return cls.__library_runner

    @classmethod
    def initialize(cls, actor, binder, sandbox_dir, output_stream):
        cls.__standalone_runner = StandaloneRunner(actor, binder, sandbox_dir, output_stream)
        cls.__library_runner = LibraryRunner(actor, binder, sandbox_dir, output_stream)

    def __init__(self, actor, binder, sandbox_dir, output_stream):
        self._actor = actor
        self._binder = binder
        self._output_stream = output_stream
        self._sandbox_dir = sandbox_dir
        self._runtime_settings = actor._ActorBaseClass__runtime_settings

    @abstractmethod
    def call_initialize(self, code_parameters: str):
        ...

    @abstractmethod
    def call_main(self, *ids_list, code_parameters: str):
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

    def __init__(self, actor, binder, sandbox_dir, output_stream):
        super().__init__( actor, binder,sandbox_dir, output_stream)

    def call_initialize(self, code_parameters: str):
        ...


    def call_finalize(self):
        ...

    @staticmethod
    def __resolve_batch_tags(cmd: str, runtime_settings:RuntimeSettings ):

        batch_settings = runtime_settings.batch
        if not batch_settings:
            return cmd

        batch_settings_dict = vars(batch_settings)
        cmd = string.Template(cmd).safe_substitute(batch_settings_dict)

        return cmd

    @staticmethod
    def __create_batch_cmd(executable, runtime_settings: RuntimeSettings):
        cmd = []

        if runtime_settings.run_mode != RunMode.BATCH:
            return executable

        batch_settings = runtime_settings.batch

        if batch_settings.batch_runner:
            cmd.append(batch_settings.batch_runner)

            if batch_settings.batch_options:
                cmd.append( batch_settings.batch_options)
        else:
            cmd.append( batch_settings.batch_default_runner )

            if batch_settings.batch_options:
                cmd.append( batch_settings.batch_options )

            if batch_settings.batch_default_options:
                cmd.append( batch_settings.batch_default_options )

        cmd = ' '.join(cmd)
        cmd = string.Template(cmd).safe_substitute(exec=executable)

        return cmd

    @staticmethod
    def __resolve_mpi_tags(cmd: str,  runtime_settings: RuntimeSettings ):

        mpi_settings = runtime_settings.mpi
        if not mpi_settings:
            cmd = StandaloneRunner.__resolve_batch_tags( cmd, runtime_settings )
            return cmd

        mpi_settings_dict = vars(mpi_settings)
        cmd = string.Template(cmd).safe_substitute(mpi_settings_dict)

        cmd = StandaloneRunner.__resolve_batch_tags(cmd, runtime_settings)
        return cmd

    @staticmethod
    def __create_mpi_cmd(executable: str, runtime_settings: RuntimeSettings ):
        cmd = []

        mpi_settings = runtime_settings.mpi
        if not mpi_settings:
            return StandaloneRunner.__create_batch_cmd(executable,runtime_settings)

        if mpi_settings.mpi_runner:
            cmd.append( mpi_settings.mpi_runner )

            if mpi_settings.mpi_options:
                cmd.append( mpi_settings.mpi_options )
        else:
            cmd.append( mpi_settings.mpi_default_runner )

            if mpi_settings.mpi_options:
                cmd.append( mpi_settings.mpi_options )

            if mpi_settings.mpi_default_options:
                cmd.append( mpi_settings.mpi_default_options )

        cmd = ' '.join(cmd)
        cmd = string.Template(cmd).safe_substitute(exec=executable)

        cmd = StandaloneRunner.__create_batch_cmd(cmd, runtime_settings)

        return cmd

    @staticmethod
    def __resolve_cmd_tags(actor, cmd: str, runtime_settings: RuntimeSettings ):

        exec = actor.actor_dir + '/bin/' + actor.name + '.exe'

        cmd = string.Template(cmd).safe_substitute(exec=exec)

        cmd = StandaloneRunner.__resolve_mpi_tags( cmd, runtime_settings )
        return cmd

    @staticmethod
    def __create_cmd(runtime_settings: RuntimeSettings):
        cmd = []

        if runtime_settings.debug_mode != DebugMode.NONE:
            if runtime_settings.debugger.debugger_cmd:
                debugger_cmd = runtime_settings.debugger.debugger_cmd
            else:
                debugger_cmd = runtime_settings.debugger.debugger_default_cmd
            cmd.append( debugger_cmd )

        if runtime_settings.exec_options:
            cmd.append( runtime_settings.exec_options )

        cmd.append( '${exec}' )

        cmd = ' '.join(cmd)
        if runtime_settings.debug_mode != DebugMode.NONE:
            StandaloneRunner.__logger.warning('While standalone debugging MPI and batch modes are switched off!')
            return cmd

        cmd = StandaloneRunner.__create_mpi_cmd(cmd, runtime_settings)

        return cmd

    def validate_command(self, exec_command:str):
        if ';' in exec_command:
            raise ValueError( 'ERROR: User provided command cannot contain ";" ' )
        if '#' in exec_command:
            raise ValueError( 'ERROR: User provided command cannot contain "#" ' )

        if '&&' in exec_command:
            raise ValueError( 'ERROR: User provided command cannot contain "&&" ' )

        if not '${exec}' in exec_command:
            raise ValueError( 'ERROR: User provided command must contain "${exec}" string ' )

    def call_main(self, *ids_list, code_parameters: str):

        self.__logger.debug( "RUNNING STDL" )

        runtime_settings = self._runtime_settings

        if runtime_settings.commandline_cmd:
            exec_command:str = runtime_settings.commandline_cmd
            if ';' in exec_command:
                raise ValueError('ERROR: User provided command cannot contain ";" ')
            if '#' in exec_command:
                raise ValueError('ERROR: User provided command cannot contain "#" ')
            if not '${exec}' in exec_command:
                raise ValueError( 'ERROR: User provided command must contain "${exec}" ' )

        else:
            exec_command = StandaloneRunner.__create_cmd(runtime_settings)

        exec_command = StandaloneRunner.__resolve_cmd_tags( self._actor,  exec_command, runtime_settings )

        print( f'COMMAND: {exec_command}' )
        # debug_mode:            exec_command.append( 'totalview' )

        results = self._binder.run_standalone( ids_list, code_parameters, exec_command,
                                                self._sandbox_dir, self._output_stream )
        return results

    def call_finalize(self):
        ...

    def call_get_state(self) -> str:
        ...

    def call_set_state(self, state: str):
        ...

    def call_get_timestamp(self) -> float:
        ...

class LibraryRunner( Runner ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self, actor, binder, sandbox_dir, output_stream):
        super().__init__( actor, binder, sandbox_dir, output_stream )

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

    def call_initialize(self, code_parameters: str):

        if self._runtime_settings.debug_mode != DebugMode.NONE:
            self.__attach_debugger()

        self._binder.call_init( code_parameters=code_parameters, sandbox_dir=self._sandbox_dir )


    def call_main(self, *ids_list, code_parameters: str):

        self.__logger.debug( "RUNNING Lib" )

        results = self._binder.call_main(ids_list, code_parameters=code_parameters, sandbox_dir=self._sandbox_dir)
        return results

    def call_finalize(self):
        self._binder.call_finish( sandbox_dir=self._sandbox_dir )

    def call_get_state(self) -> str:
        state = self._binder.call_get_state( sandbox_dir=self._sandbox_dir )
        return state

    def call_set_state(self, state: str):
        self._binder.call_set_state(state,  sandbox_dir=self._sandbox_dir )

    def call_get_timestamp(self) -> float:
        timestamp = self._binder.call_get_timestamp( sandbox_dir=self._sandbox_dir )
        return timestamp
