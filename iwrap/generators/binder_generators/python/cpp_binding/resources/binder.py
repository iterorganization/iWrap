import ctypes
import os
import logging
import subprocess
import sys
from pathlib import Path
from threading import Thread

import imas
from .data_storages import IDSConvertersRegistry

from ..common.definitions import Argument

from .data_type import LegacyIDSConverter
from .data_c_binding import ParametersCType, StatusCType
from ..common.runtime_settings import RuntimeSettings, RunMode, DebugMode


def exec_system_cmd(system_cmd: str,  output_stream=sys.stdout) :

    proc = subprocess.Popen( system_cmd, shell=True,
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

        self.ids_converter_class = None
        self.actor = None

        self.actor_dir = None

        self.runtime_settings : RuntimeSettings = None
        self.arg_metadata_list = None

        self.wrapper_init_func = None
        self.wrapper_main_func = None
        self.wrapper_finish_func = None



    def initialize(self, actor):

        IDSConvertersRegistry.initialize()
        self.ids_converter_class = IDSConvertersRegistry.get_converter_class(actor.data_type, 'cpp')
        self.actor = actor

        self.runtime_settings = actor.runtime_settings
        self.arg_metadata_list = actor.arguments
        self.actor_dir = actor.actor_dir

        self.ids_converter_class.initialize(actor.is_standalone_run(), self.runtime_settings.ids_storage)
        actor_name = self.actor.name
        if self.actor.code_description['implementation']['subroutines'].get('init'):
            sbrt_name = 'init_' + actor_name + "_wrapper"
            self.wrapper_init_func = self.__get_wrapper_function(sbrt_name)

        sbrt_name = actor_name + "_wrapper"
        self.wrapper_main_func = self.__get_wrapper_function(sbrt_name)


        if self.actor.code_description['implementation']['subroutines'].get('finalize'):
            sbrt_name = 'finish_' + actor_name + "_wrapper"
            self.wrapper_finish_func = self.__get_wrapper_function(sbrt_name)

    def finalize(self):
        self.ids_converter_class.finalize()

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

    def get_converters(self, ids_list, code_parameters):

        ids_converters_list = None
        if ids_list:
            input_idses = list( ids_list )

            ids_converters_list = []

            # LOOP over ids

            for arg_meta_data in self.arg_metadata_list:
                ids_value = None
                if arg_meta_data.intent == Argument.IN:
                    ids_value = input_idses.pop( 0 )

                arg = self.ids_converter_class( arg_meta_data.type, arg_meta_data.intent, ids_value )
                ids_converters_list.append( arg )

        # XML Code Params
        param_c = None
        if code_parameters:
            param_c = ParametersCType( code_parameters )


        # DIAGNOSTIC INFO
        status_info_converter = StatusCType()

        return ids_converters_list, param_c, status_info_converter

    def get_native_arguments(self, ids_converters_list, code_parameters_converter, status_info_converter):

        c_arglist = []
        if ids_converters_list:
            c_idslist = [arg.convert_to_native_type() for arg in ids_converters_list]
            c_arglist = c_arglist + c_idslist

        if code_parameters_converter:
            c_code_parameters = code_parameters_converter.convert_to_native_type()
            c_arglist.append(c_code_parameters)

        c_status_info = status_info_converter.convert_to_native_type()
        c_arglist.append( c_status_info )

        return c_arglist

    def __save_input(self, full_arguments_list, code_parameters, sandbox_dir):

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
            ParametersCType.save(code_parameters, file)

    def __read_output(self, status_info, sandbox_dir):

        file_path = Path(sandbox_dir, 'output.txt')
        with open( file_path, "rt" ) as file:
            # Read status info
            status_info.read(file)

    def run_standalone(self, ids_list, code_parameters, exec_command, sandbox_dir:str, output_stream):

        self.__logger.debug( "RUNNING STDL" )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)

        ids_converters_list, code_parameters_converter, status_info_converter = self.get_converters( ids_list, code_parameters)
        c_arglist = self.get_native_arguments( ids_converters_list, code_parameters_converter, status_info_converter )
        self.__save_input( ids_converters_list, code_parameters_converter, sandbox_dir )
        self.__logger.debug( 'EXECUTING command: ' + str(exec_command) )
        exec_system_cmd(exec_command, output_stream=output_stream)

        # go back to initial dir
        os.chdir(cwd)

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info_converter )

        # get output data
        results = []
        for arg in ids_converters_list:
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

    def call_init(self, code_parameters: str, sandbox_dir: str, debug_mode=False):

        if not self.wrapper_init_func:
            return

        _, code_parameters_converter, status_info_converter = self.get_converters( None, code_parameters)
        c_arglist = self.get_native_arguments( _, code_parameters_converter, status_info_converter )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)

        # call INIT
        self.wrapper_init_func( *c_arglist )

        # go back to initial dir
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info_converter )

    # only input arguments, outputs are returned (as a list if more than 1)
    def call_main(self, input_idses, code_parameters:str, sandbox_dir:str):
        """
        """
        print( 'RUN MODE: ', str( self.runtime_settings.run_mode ) )
        ids_converters_list, code_parameters_converter, status_info_converter = self.get_converters( input_idses, code_parameters)

        c_arglist = self.get_native_arguments( ids_converters_list, code_parameters_converter, status_info_converter )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir( sandbox_dir )



        self.wrapper_main_func( *c_arglist )
        # go back to initial dir
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check(status_info_converter)

        # get output data
        results = []
        for arg in ids_converters_list:
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

        _, _, status_info_converter = self.get_converters( ids_list=None, code_parameters=None)

        c_arg_list = self.get_native_arguments(_, _, status_info_converter )

        # go to sandbox
        cwd = os.getcwd()
        os.chdir(sandbox_dir)

        # call FINISH
        self.wrapper_finish_func( *c_arg_list )

        # go back to initial dir
        os.chdir( cwd )

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info_converter )
