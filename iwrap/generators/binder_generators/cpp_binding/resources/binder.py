import ctypes
import os
import logging
import subprocess
import sys
from pathlib import Path
from threading import Thread

import imas

from ..common.definitions import Argument

from .data_type import LegacyIDS
from .data_c_binding import ParametersCType, StatusCType
from ..common.runtime_settings import RuntimeSettings, RunMode, DebugMode


def exec_system_cmd(system_cmd: str,  output_stream=sys.stdout) :

    proc = subprocess.Popen( system_cmd,
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

    for line in proc.stdout:
        line = line.decode( errors='replace' )
        print( line, end='', file=output_stream )

    return_code = proc.wait()
    if return_code:
        raise RuntimeError( f'ERROR [{return_code}] while executing command: {system_cmd}' )


class CBinder:

    def __init__(self):
        self.__logger = logging.getLogger( 'binding' )
        self.__logger.setLevel( logging.DEBUG )

        self.work_db = None
        self.actor = None

        self.actor_dir = None

        self.runtime_settings : RuntimeSettings = None
        self.formal_arguments = None

        self.wrapper_init_func = None
        self.wrapper_main_func = None
        self.wrapper_finish_func = None

    def save_data(self, ids):
        pass

    def read_data(self, ids):
        pass

    def initialize(self, actor):
        self.actor = actor

        self.runtime_settings = actor.runtime_settings
        self.formal_arguments = actor.arguments
        self.actor_dir = actor.actor_dir
        self.work_db = self.__create_work_db()

        actor_name = self.actor.name
        if self.actor.code_description['subroutines'].get('init'):
            sbrt_name = 'init_' + actor_name + "_wrapper"
            self.wrapper_init_func = self.__get_wrapper_function(sbrt_name)

        sbrt_name = actor_name + "_wrapper"
        self.wrapper_main_func = self.__get_wrapper_function(sbrt_name)


        if self.actor.code_description['subroutines'].get('finish'):
            sbrt_name = 'finish_' + actor_name + "_wrapper"
            self.wrapper_finish_func = self.__get_wrapper_function(sbrt_name)



    def finalize(self):
        ...


    def __create_work_db(self):
        ids_storage = self.runtime_settings.ids_storage
        is_standalone_run = self.actor.is_standalone_run()

        if is_standalone_run and ids_storage.backend is imas.imasdef.MEMORY_BACKEND:
            backend = ids_storage.persistent_backend
        else:
            backend = ids_storage.backend

        db_entry = imas.DBEntry( backend_id=backend,
                                 db_name=ids_storage.db_name,
                                 shot=ids_storage.shot,
                                 run=ids_storage.run )

        db_entry.create()
        return db_entry

    def __get_wrapper_function(self, function_name: str):

        actor_name = self.actor.name
        lib_path = self.actor_dir + '/lib/lib' + actor_name + '.so'

        wrapper_lib = ctypes.CDLL( lib_path )
        wrapper_fun = getattr( wrapper_lib, function_name )
        return wrapper_fun

    def __status_check(self, status_info):

        actor_name = self.actor.name
        if status_info.code < 0:
            raise Exception(
                "Actor *** '" + actor_name + "' *** returned an error (" + str( status_info.code ) + "): '"
                + status_info.message + "'" )

        if status_info.code > 0:
            self.__logger.warning(
                "Actor * '" + actor_name + "' * returned diagnostic info: \n     Output flag:      ",
                status_info.code, "\n     Diagnostic info: ", status_info.message )

    def __attach_debugger(self, sbrt_name:str):


        def start_debugger():
            process_id = os.getpid()
            tv_command = ['totalview',
                          '-e', 'dset VERBOSE warning',
                          '-e', 'dset TV::dll_read_loader_symbols_only *',
                          '-e', 'dset TV::GUI::pop_at_breakpoint true',
                          '-e', f'dattach python {process_id}',
                          '-e', f'dbreak -pending {sbrt_name}',
                          '-e', 'puts  "\n\nTotalView attached to a running Python process.\n"',
                          '-e', 'puts  "Press any key to continue!\n"',
                          '-e', 'puts  "WARNING:\tRestarting or killing debugged process will close the workflow!"',
                          ]

            exec_system_cmd(tv_command, output_stream=self.actor.output_stream)

        t = Thread( target=start_debugger, args=() )
        t.daemon = True  # thread dies with the program
        t.start()
        input()  # just to wait until debugger starts

    def __run_normal(self, c_arglist, sandbox_dir:str, debug_mode=False):

        sbrt_name = self.actor.name + "_wrapper"
        if debug_mode:
            self.__attach_debugger(sbrt_name)

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)

        self.wrapper_main_func( *c_arglist )
        # go back to initial dir
        os.chdir(cwd)

    def __save_input(self, full_arguments_list, sandbox_dir):

        file_path = Path(sandbox_dir, 'input.txt')
        with open( file_path, "wt" ) as file:
            # Save IDS arguments
            file.write( ' Arguments '.center(70, '=') )
            file.write( "\n" )
            file.write( 'Length:' )
            file.write( "\n" )
            file.write( str( len( full_arguments_list ) ) + "\n" )
            for arg in full_arguments_list:
                arg.save( file )
            self.actor.code_parameters.save(file)

    def __run_standalone(self, full_arguments_list, sandbox_dir:str, mpi_settings=None, debug_mode=False):

        self.__logger.debug( "RUNNING STDL" )

        exec_command = []

        if self.actor.is_mpi_code and mpi_settings:
            exec_command.append( 'mpiexec' )
            np = mpi_settings.number_of_processes
            if np and str( np ).isnumeric():
                exec_command.append( '-np' )
                exec_command.append( str( np ) )
            if debug_mode:
                exec_command.append( '-tv' )
        elif debug_mode:
            exec_command.append( 'totalview' )

        exec_command.append( self.actor_dir + '/bin/' + self.actor.name + '.exe' )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)
        self.__save_input( full_arguments_list , sandbox_dir)

        self.__logger.debug( 'EXECUTING command: ', exec_command )
        exec_system_cmd(exec_command, output_stream=self.actor.output_stream)

        # go back to initial dir
        os.chdir(cwd)

    def call_init(self, code_parameters: str, sandbox_dir: str, debug_mode=False):

        if not self.wrapper_init_func:
            return

        c_arglist = []

        # XML Code Params
        if code_parameters:
            param_c = ParametersCType(code_parameters).convert_to_native_type()
            c_arglist.append( param_c )

        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)

        # call INIT
        self.wrapper_init_func( *c_arglist )

        # go back to initial dir
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )

    # only input arguments, outputs are returned (as a list if more than 1)
    def call_main(self, *input_idses, code_parameters:str, sandbox_dir:str):
        """
        """
        input_idses = list( input_idses )

        full_arguments_list = []

        # LOOP over full_arguments_list
        for formal_arg in self.formal_arguments:
            ids_value = None
            if formal_arg.intent == Argument.IN:
                ids_value = input_idses.pop( 0 )

            arg = LegacyIDS( self.work_db, formal_arg, ids_value )
            full_arguments_list.append( arg )
            pass

        #
        c_arglist = [arg.convert_to_native_type() for arg in full_arguments_list]

        # XML Code Params
        if code_parameters:
            param_c = ParametersCType(code_parameters).convert_to_native_type()
            c_arglist.append( param_c )

        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        print( 'RUN MODE: ', str( self.runtime_settings.run_mode ) )

        mpi_settings = self.runtime_settings.mpi
        is_standalone =  self.actor.is_standalone_run()
        debug_mode = self.runtime_settings.debug_mode != DebugMode.NONE

        if is_standalone:
            self.__run_standalone( full_arguments_list, sandbox_dir=sandbox_dir,
                                   mpi_settings=mpi_settings, debug_mode=debug_mode )
        else:
            self.__run_normal( c_arglist,  sandbox_dir=sandbox_dir, debug_mode=debug_mode )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )

        # get output data
        results = []
        for arg in full_arguments_list:
            if arg.intent == Argument.OUT:
                results.append( arg.convert_to_actor_type() )
            arg.release()

        # final output
        if not results:
            return None
        elif len( results ) == 1:
            return results[0]
        else:
            return tuple( results )

    def call_finish(self, sandbox_dir: str):
        if not self.wrapper_finish_func:
            return

        c_arglist = []
        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)

        # call FINISH
        self.wrapper_finish_func( *c_arglist )

        # go back to initial dir
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )
