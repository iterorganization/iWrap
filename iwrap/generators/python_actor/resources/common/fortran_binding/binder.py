import ctypes
import os
import logging
import subprocess
from threading import Thread

import imas

from ..definitions import Argument
from ..code_parameters import CodeParameters

from .data_type import LegacyIDS
from .data_c_binding import ParametersCType, StatusCType
from ..runtime_settings import RuntimeSettings, RunMode, DebugMode


class FortranBinder:

    def __init__(self, actor_dir, actor_name, native_language, code_name, is_mpi_code):
        self.logger = logging.getLogger( 'binding' )
        self.logger.setLevel( logging.DEBUG )

        self.actor_dir = actor_dir
        self.actor_name = actor_name
        self.code_name = code_name + '_wrapper'
        self.is_mpi_code = is_mpi_code

        self.wrapper_dir = self.actor_dir + '/' + native_language + '_wrapper'
    def save_data(self, ids):
        pass

    def read_data(self, ids):
        pass

    def initialize(self, runtime_settings: RuntimeSettings, arguments, codeparams: CodeParameters):
        self.runtime_settings = runtime_settings
        self.code_parameters = codeparams
        self.formal_arguments = arguments

        self.work_db = self.__create_work_db()
        self.wrapper_func = self.__get_wrapper_function()

    def __create_work_db(self):
        ids_storage = self.runtime_settings.ids_storage
        is_standalone_run = self.runtime_settings.debug_mode is DebugMode.STANDALONE \
                            or self.runtime_settings.run_mode is RunMode.STANDALONE

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

    def __get_wrapper_function(self):

        lib_path = self.wrapper_dir + '/lib/lib' + self.actor_name + '.so'

        wrapper_lib = ctypes.CDLL( lib_path )
        wrapper_fun = getattr( wrapper_lib, self.code_name )
        return wrapper_fun

    def __status_check(self, status_info):

        if status_info.code < 0:
            raise Exception(
                "Actor *** '" + self.actor_name + "' *** returned an error (" + str( status_info.code ) + "): '"
                + status_info.message + "'" )

        if status_info.code > 0:
            self.logger.warning(
                "Actor * '" + self.actor_name + "' * returned diagnostic info: \n     Output flag:      ",
                status_info.code, "\n     Diagnostic info: ", status_info.message )

    def __attach_debugger(self):

        process_id = os.getpid()

        tv_command = ['totalview',
                      '-e', 'dset VERBOSE warning',
                      '-e', 'dset TV::dll_read_loader_symbols_only *',
                      '-e', 'dset TV::GUI::pop_at_breakpoint true',
                      '-e', f'dattach python {process_id}',
                      '-e', f'dbreak -pending {self.code_name}',
                      '-e', 'puts  "\n\nTotalView attached to a running Python process.\n"',
                      '-e', 'puts  "Press any key to continue!\n"',
                      '-e', 'puts  "WARNING:\tRestarting or killing debugged process will close the workflow!"',
                      ]

        proc = subprocess.Popen( tv_command,
                                 encoding='utf-8', text=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout:
            print( line, end='' )

        return_code = proc.wait()

    def __run_normal(self, c_arglist, debug_mode=False):
        if debug_mode:
            t = Thread( target=self.__attach_debugger, args=() )
            t.daemon = True  # thread dies with the program
            t.start()
            input()  # just to wait until debugger starts

        self.wrapper_func( *c_arglist )

    def __save_input(self, full_arguments_list):

        file = open( 'input.txt', "w" )
        file.write( str( len( full_arguments_list ) ) + "\n" )
        for arg in full_arguments_list:
            arg.save( file )

        file.close()

    def __run_standalone(self, full_arguments_list, mpi_settings=None, debug_mode=False):

        print( "RUNNING STDL" )

        exec_command = []

        self.__save_input( full_arguments_list )

        if self.is_mpi_code and mpi_settings:
            exec_command.append( 'mpiexec' )
            np = mpi_settings.number_of_processes
            if np and str(np).isnumeric():
                exec_command.append( '-np' )
                exec_command.append( str(np) )
            if debug_mode:
                exec_command.append( '-tv' )
        elif debug_mode:
                exec_command.append( 'totalview' )

        exec_command.append( './bin/' + self.actor_name + '.exe' )

        print('EXEC command: ', exec_command)
        proc = subprocess.Popen( exec_command,
                                 encoding='utf-8', text=True,
                                 stdout=subprocess.PIPE, stderr=subprocess.STDOUT )

        for line in proc.stdout:
            print( line, end='' )

        return_code = proc.wait()

    # only input arguments, outputs are returned (as a list if more than 1)
    def call_native_code(self, *input_idses):
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
        if self.code_parameters.parameters:
            param_c = ParametersCType( self.code_parameters ).convert_to_native_type()
            c_arglist.append( param_c )

        # DIAGNOSTIC INFO
        status_info = StatusCType()
        c_arglist.append( status_info.convert_to_native_type() )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir( self.wrapper_dir)

        print('RUN MODe: ' , str(self.runtime_settings.run_mode))

        mpi_settings = self.runtime_settings.mpi
        # call the NATIVE function
        if self.runtime_settings.debug_mode is DebugMode.ATTACH:
            self.__run_normal( c_arglist, debug_mode=True )
        elif self.runtime_settings.debug_mode is DebugMode.STANDALONE:
            self.__run_standalone( full_arguments_list, mpi_settings=mpi_settings, debug_mode=True )
        elif self.runtime_settings.run_mode is RunMode.NORMAL:
            self.__run_normal( c_arglist, debug_mode=False )
        elif self.runtime_settings.run_mode is RunMode.STANDALONE:
            self.__run_standalone( full_arguments_list, mpi_settings=mpi_settings, debug_mode=False )
        else:
            raise ValueError( 'ERROR! Unknown "run_mode" value!' )

        # go back from sandbox to original location
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info )



        # get output data
        results = []
        for arg in full_arguments_list:
            if arg.intent == Argument.OUT:
                results.append( arg.convert_to_actor_type() )

        # final output
        if not results:
            return None
        elif len( results ) == 1:
            return results[0]
        else:
            return tuple( results )
