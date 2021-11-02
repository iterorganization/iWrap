import ctypes
import os
import logging
import subprocess
from pathlib import Path
from threading import Thread

import imas

from iwrap.common import utils
from ..definitions import Argument

from .data_type import LegacyIDS
from .data_c_binding import ParametersCType, StatusCType
from ..runtime_settings import RuntimeSettings, RunMode, DebugMode, SandboxLifeTime
from ..sandbox import Sandbox


class CBinder:

    def __init__(self):
        self.__logger = logging.getLogger( 'binding' )
        self.__logger.setLevel( logging.DEBUG )

        self.work_db = None
        self.actor = None
        self.sandbox = None
        self.actor_dir = None

        self.runtime_settings : RuntimeSettings = None
        self.formal_arguments = None

        self.wrapper_init_func = None
        self.wrapper_main_func = None
        self.wrapper_finish_func = None

    def is_standalone_run(self):
        if self.actor.is_mpi_code:
            return True

        if self.runtime_settings.debug_mode == DebugMode.STANDALONE:
            return True

        if self.runtime_settings.run_mode == RunMode.STANDALONE:
            return True

        return False

    def save_data(self, ids):
        pass

    def read_data(self, ids):
        pass

    def initialize(self, actor):
        self.actor = actor
        self.sandbox = Sandbox(self.actor)
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

        self.sandbox.initialize()

        if not self.is_standalone_run():
            self.__run_init()

    def finalize(self):
        if not self.is_standalone_run():
            self.__run_finalize()

        self.sandbox.remove()


    def __create_work_db(self):
        ids_storage = self.runtime_settings.ids_storage
        is_standalone_run = self.is_standalone_run()

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
        wrapper_dir = self.actor_dir + '/' + self.actor.native_language + '_wrapper'
        lib_path =  self.actor_dir + '/lib/lib' + actor_name + '.so'

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

    def __attach_debugger(self):

        process_id = os.getpid()

        tv_command = ['totalview',
                      '-e', 'dset VERBOSE warning',
                      '-e', 'dset TV::dll_read_loader_symbols_only *',
                      '-e', 'dset TV::GUI::pop_at_breakpoint true',
                      '-e', f'dattach python {process_id}',
                      #'-e', f'dbreak -pending {self.main_sbrt_name}',
                      '-e', 'puts  "\n\nTotalView attached to a running Python process.\n"',
                      '-e', 'puts  "Press any key to continue!\n"',
                      '-e', 'puts  "WARNING:\tRestarting or killing debugged process will close the workflow!"',
                      ]

        proc = subprocess.Popen( tv_command,
                                 stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout:
            line = line.decode(errors='replace')
            print( line, end='' )

        return_code = proc.wait()
        if return_code:
            raise RuntimeError(f'ERROR [{return_code}] while executing command: {tv_command}')

    def __run_init(self, debug_mode=False):

        if not self.wrapper_init_func:
            return

        c_arglist = []

        # XML Code Params
        if self.actor.code_parameters and self.actor.code_parameters.parameters:
            param_c = ParametersCType( self.actor.code_parameters ).convert_to_native_type()
            c_arglist.append( param_c )

        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(self.sandbox.path)

        # call INIT
        self.wrapper_init_func( *c_arglist )

        # go back to initial dir
        os.chdir(cwd)

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )

    def __run_finalize(self):
        if not self.wrapper_finish_func:
            return

        c_arglist = []
        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(self.sandbox.path)

        # call FINISH
        self.wrapper_finish_func( *c_arglist )

        # go back to initial dir
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )

        self.sandbox.clean()

    def __run_normal(self, c_arglist, debug_mode=False):
        if debug_mode:
            t = Thread( target=self.__attach_debugger, args=() )
            t.daemon = True  # thread dies with the program
            t.start()
            input()  # just to wait until debugger starts

        self.wrapper_main_func( *c_arglist )

    def __save_input(self, full_arguments_list):

        file_path = Path(self.sandbox.path, 'input.txt')
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

    def __run_standalone(self, full_arguments_list, mpi_settings=None, debug_mode=False):

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
        os.chdir(self.sandbox.path)
        self.__save_input( full_arguments_list )

        self.__logger.debug( 'EXECUTING command: ', exec_command )
        proc = subprocess.Popen( exec_command,
                                 stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout:
            line = line.decode(errors='replace')
            print( line, end='' )

        return_code = proc.wait()
        if return_code:
            raise RuntimeError(f'ERROR [{return_code}] while executing command: {exec_command}')

        # go back to initial dir
        os.chdir(cwd)

    # only input arguments, outputs are returned (as a list if more than 1)
    def step(self, *input_idses):
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
        if self.actor.code_parameters and self.actor.code_parameters.parameters:
            param_c = ParametersCType( self.actor.code_parameters ).convert_to_native_type()
            c_arglist.append( param_c )

        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(self.sandbox.path)

        print( 'RUN MODE: ', str( self.runtime_settings.run_mode ) )

        mpi_settings = self.runtime_settings.mpi
        is_standalone =  self.is_standalone_run()
        debug_mode = self.runtime_settings.debug_mode != DebugMode.NONE

        if is_standalone:
            self.__run_standalone( full_arguments_list, mpi_settings=mpi_settings, debug_mode=debug_mode )
        else:
            self.__run_normal( c_arglist, debug_mode=debug_mode )


        # go back from sandbox to original location
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )

        # get output data
        results = []
        for arg in full_arguments_list:
            if arg.intent == Argument.OUT:
                results.append( arg.convert_to_actor_type() )
            arg.release()


        if self.runtime_settings.sandbox.life_time == SandboxLifeTime.ACTOR_RUN:
            self.sandbox.clean()

        # final output
        if not results:
            return None
        elif len( results ) == 1:
            return results[0]
        else:
            return tuple( results )

