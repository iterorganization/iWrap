import ctypes
import os
import logging
import subprocess
import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any, List

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

class Binder (ABC):

    @abstractmethod
    def initialize(self, actor) -> None:
        ...

    @abstractmethod
    def finalize(self) -> None:
        ...

    @abstractmethod
    def call_init(self, code_parameters:str):
        ...

    @abstractmethod
    def call_main(self, *input_idses, code_parameters:str):
        ...

    @abstractmethod
    def call_finish(self):
        ...

    @abstractmethod
    def call_set_state(self, state:str):
        ...

    @abstractmethod
    def call_get_state(self) -> None:
        ...

    @abstractmethod
    def run_standalone(self, ids_list:List[Any], code_parameters:str, exec_command:str, sandbox_dir:str, output_stream) -> None:
        ...


class CBinder(Binder):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):
        self.ids_converter = None
        self.actor = None

        self.actor_dir = None

        self.runtime_settings: RuntimeSettings = None
        self.arg_metadata_list = None

        self.wrapper_init_func = None
        self.wrapper_main_func = None
        self.wrapper_finish_func = None

        self.wrapper_set_state_func = None
        self.wrapper_get_state_func = None

        self.wrapper_get_timestamp_func = None

    def initialize(self, actor):
        self.actor = actor

        IDSConvertersRegistry.initialize()
        data_type = self.actor.code_description['implementation']['data_type']
        self.ids_converter = IDSConvertersRegistry.get_converter(data_type, 'cpp')

        self.runtime_settings = actor.get_runtime_settings()
        self.ids_ctype_list = None
        self.actor_dir = actor.actor_dir

        sandbox_dir = self.actor.sandbox.path
        self.ids_converter.initialize(sandbox_dir, actor.is_standalone_run(), self.runtime_settings.ids_storage)
        actor_name = self.actor.name.lower()
        subroutines = self.actor.code_description['implementation']['subroutines']

        if subroutines.get('init'):
            sbrt_name = actor_name + "_wrapper_init"
            self.wrapper_init_func = self.__get_wrapper_function(sbrt_name)

        sbrt_name = actor_name + "_wrapper_main"
        self.wrapper_main_func = self.__get_wrapper_function(sbrt_name)

        if subroutines.get('finalize'):
            sbrt_name = actor_name + "_wrapper_finalize"
            self.wrapper_finish_func = self.__get_wrapper_function(sbrt_name)

        if subroutines.get( 'get_state' ):
            sbrt_name = actor_name + "_wrapper_get_state"
            self.wrapper_get_state_func = self.__get_wrapper_function( sbrt_name )

        if subroutines.get( 'set_state' ):
            sbrt_name = actor_name + "_wrapper_set_state"
            self.wrapper_set_state_func = self.__get_wrapper_function( sbrt_name )

        if subroutines.get( 'get_timestamp' ):
            sbrt_name = actor_name + "_wrapper_get_timestamp"
            self.wrapper_get_timestamp_func = self.__get_wrapper_function( sbrt_name )


    def finalize(self):
        self.ids_converter.finalize()

    def __check_inputs(self, ids_arguments_list, arg_metadata_list):
        import functools

        inputs_number = functools.reduce(lambda nbr, arg: nbr + 1 if arg['intent'] == Argument.IN else nbr,
                                         arg_metadata_list, 0 )

        # check if a number of provided arguments is correct
        if inputs_number != len(ids_arguments_list):
            raise RuntimeError(f'Wrong number of arguments (received: {len(ids_arguments_list)}, expected: {inputs_number})')


    def __get_wrapper_function(self, function_name: str):

        actor_name = self.actor.name
        lib_path = self.actor_dir + '/lib/lib' + actor_name + '.so'

        wrapper_lib = ctypes.CDLL( lib_path )
        wrapper_fun = getattr( wrapper_lib, function_name )
        return wrapper_fun

    def __status_check(self, status_info: StatusCType):

        actor_name = self.actor.name
        if status_info.code < 0:
            raise Exception(
                "Actor *** '" + actor_name + "' *** returned an error (" + str( status_info.code ) + "): '"
                + status_info.message + "'" )

        if status_info.code > 0:
            self.__logger.warning(
                "Actor * '" + actor_name + "' * returned diagnostic info: \n     Output flag:      " +
                str(status_info.code) + "\n     Diagnostic info: " + status_info.message )

    def __get_ids_ctypes(self, arg_metadata_list):

        ids_ctype_list = []

        # LOOP over ids
        for arg_meta_data in arg_metadata_list:
            ids_ctype = self.ids_converter.prepare_native_type( arg_meta_data['type'] )
            ids_ctype.intent = arg_meta_data['intent']
            ids_ctype_list.append( ids_ctype )

        return ids_ctype_list

    def __save_input(self, method_name, full_arguments_list, code_parameters, sandbox_dir):

        file_path = Path(sandbox_dir, f'{method_name}.in')
        with open( file_path, "wt" ) as file:
            # Save IDS arguments
            file.write( ' Arguments '.center(70, '=') )
            file.write( "\n" )
            file.write( 'Length:' )
            file.write( "\n" )
            file.write( str( len( full_arguments_list ) ) + "\n" )
            for arg in full_arguments_list:
                arg.save( file )

            if code_parameters:
                code_parameters.save(sandbox_dir)

    def __read_output(self, method_name, status_info, sandbox_dir):

        file_path = Path(sandbox_dir, f'{method_name}.out')
        with open( file_path, "rt", errors='replace' ) as file:
            # Read status info
            status_info.read(file)

    def run_standalone(self, *input_idses, method_name, arg_metadata_list, code_parameters, exec_command, sandbox_dir:str, output_stream):

        self.__logger.debug( "RUNNING STDL" )
        # if arg_metadata_list:
        # check if a number of provided arguments is correct
        self.__check_inputs(input_idses, arg_metadata_list)

        ids_ctypes_list = self.__get_ids_ctypes(arg_metadata_list)

        tmp_ids_list = list(input_idses)
        for ids_ctype in ids_ctypes_list:
            if ids_ctype.intent == Argument.IN:
                ids_object = tmp_ids_list.pop(0)
                self.ids_converter.convert_to_native_type(ids_ctype, ids_ctype.intent, ids_object)


        param_ctype = ParametersCType(code_parameters) if code_parameters  else  None
        status_info_ctype = StatusCType()

        # prepares input files
        self.__save_input( method_name, ids_ctypes_list, param_ctype, sandbox_dir )
        self.__logger.debug( 'EXECUTING command: ' + str(exec_command) )
        exec_system_cmd(exec_command, output_stream=output_stream)

        # Checking returned DIAGNOSTIC INFO
        self.__read_output(method_name, status_info_ctype, sandbox_dir)
        self.__status_check( status_info_ctype )

        # get output data
        results = []
        for ids_ctype in ids_ctypes_list:
            if ids_ctype.intent == Argument.OUT:
                results.append(self.ids_converter.convert_to_actor_type(ids_ctype))
            self.ids_converter.release(ids_ctype)

        # final output
        if not results:
            return None
        elif len( results ) == 1:
            return results[0]
        else:
            return tuple( results )

    def call_init(self, code_parameters: str):

        self.call_basic_method( self.wrapper_init_func, [], None,
                                         code_parameters=code_parameters )


    # only input arguments, outputs are returned (as a list if more than 1)
    def call_main(self, *input_idses, code_parameters:str):
        """
        """
        arg_metadata_list = self.actor.code_description.get('arguments')
        output = self.call_basic_method( self.wrapper_main_func, arg_metadata_list, *input_idses,
                                         code_parameters=code_parameters)
        return output

    def call_finish(self):

        self.call_basic_method(self.wrapper_finish_func, [], None,
                                        code_parameters=None)


    def call_basic_method(self, basic_method, arg_metadata_list,
                          *input_idses, code_parameters = None):
        """
        """

        if not basic_method:
            return

        c_arglist = []
        self.__logger.debug( 'RUN MODE: ' + str( self.runtime_settings.run_mode ) )

        ids_ctypes_list = self.__get_ids_ctypes( arg_metadata_list )

        if arg_metadata_list:
            # check if a number of provided arguments is correct
            self.__check_inputs( input_idses, arg_metadata_list )

            tmp_ids_list = list( input_idses )
            for ids_ctype in ids_ctypes_list:
                ids_object = None
                if ids_ctype.intent == Argument.IN:
                    ids_object = tmp_ids_list.pop( 0 )

                c_ids = self.ids_converter.convert_to_native_type( ids_ctype, ids_ctype.intent, ids_object )
                c_arglist.append( c_ids )

        # Code Parameterss
        if code_parameters:
            param_ctype = ParametersCType( code_parameters )
            c_param = param_ctype.convert_to_native_type()
            c_arglist.append( c_param )

        # Add status info to argument list
        status_info_ctype = StatusCType()
        c_status_info = status_info_ctype.convert_to_native_type()
        c_arglist += c_status_info

        # call native MAIN method of wrapper
        basic_method( *c_arglist )

        # Checking returned DIAGNOSTIC INFO
        status_info_ctype.convert_to_actor_type( c_arglist[-2], c_arglist[-1] )
        self.__status_check( status_info_ctype )

        # get output data
        results = []
        for ids_ctype in ids_ctypes_list:
            if ids_ctype.intent == Argument.OUT:
                results.append( self.ids_converter.convert_to_actor_type( ids_ctype ) )
            self.ids_converter.release( ids_ctype )

        # final output
        if not results:
            return None
        elif len( results ) == 1:
            return results[0]
        else:
            return tuple( results )

    def call_set_state(self, state:str):
        if not self.wrapper_set_state_func:
            return

        if not state:
            return

        encoded_state = state.encode('utf-8')

        cref_state = ctypes.c_char_p(encoded_state)
        state_size = len(encoded_state)
        cref_state_size = ctypes.byref(ctypes.c_int(state_size))

        # Add status info to argument list
        status_info_ctype = StatusCType()
        cref_code, cref_msg = status_info_ctype.convert_to_native_type()

        # call FINISH
        self.wrapper_set_state_func(cref_state, cref_state_size,  cref_code, cref_msg)

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info_ctype )

    def call_get_state(self) -> str:
        if not self.wrapper_get_state_func:
            return None

        state = None

        c_ptr_state = ctypes.c_char_p()
        cref_state = ctypes.pointer( c_ptr_state )

        # Add status info to argument list
        status_info_ctype = StatusCType()
        cref_code, cref_msg = status_info_ctype.convert_to_native_type()

        # call native MAIN method of wrapper
        self.wrapper_get_state_func( cref_state, cref_code, cref_msg )

        # Checking returned DIAGNOSTIC INFO
        status_info_ctype.convert_to_actor_type( cref_code, cref_msg )
        self.__status_check( status_info_ctype )

        state_raw = cref_state.contents.value
        if state_raw:
            state = state_raw.decode('utf-8','replace')

        return state

    def call_get_timestamp(self) -> float:
        if not self.wrapper_get_timestamp_func:
            return None

        c_double_timestamp = ctypes.c_double( 0.0 )
        cref_timestamp = ctypes.pointer( c_double_timestamp )

        # Add status info to argument list
        status_info_ctype = StatusCType()
        cref_code, cref_msg = status_info_ctype.convert_to_native_type()

        # call native MAIN method of wrapper
        self.wrapper_get_timestamp_func( cref_timestamp, cref_code, cref_msg )

        # Checking returned DIAGNOSTIC INFO
        status_info_ctype.convert_to_actor_type( cref_code, cref_msg )
        self.__status_check( status_info_ctype )

        timestamp = cref_timestamp.contents.value

        return timestamp

