import ctypes
import os
import logging

from .data_storages import IDSConvertersRegistry

from ..common.definitions import Argument
from ..common.binder import Binder

from .data_c_binding import ParametersCType, StatusCType, IDSCType
from ..common.runtime_settings import RuntimeSettings, RunMode, DebugMode
from ..common import exec_system_cmd

class LanguageBinder(Binder):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def standalone_cmd(self, method_name:str) -> str:
        exec_cmd = self.actor.actor_dir + '/bin/' + self.actor.name + '_' + method_name + '.exe'
        return exec_cmd

    def __init__(self):
        self.ids_converter = None
        self.actor = None

        self.actor_dir = None

        self.runtime_settings: RuntimeSettings = None
        self.arg_metadata_list = None

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

        subroutines = self.actor.code_description['implementation']['subroutines']

        if not subroutines.get(function_name):
            return None

        sbrt_name = actor_name + "_wrapper_" + function_name

        wrapper_fun = getattr( wrapper_lib, sbrt_name )
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
            ids_ctype = self.ids_converter.prepare_native_type(IDSCType, arg_meta_data['type'] )
            ids_ctype.intent = arg_meta_data['intent']
            ids_ctype_list.append( ids_ctype )

        return ids_ctype_list

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
        Binder.save_input( method_name, ids_ctypes_list, param_ctype, sandbox_dir )
        self.__logger.debug( 'EXECUTING command: ' + str(exec_command) )
        exec_system_cmd(exec_command, output_stream=output_stream)

        # Checking returned DIAGNOSTIC INFO
        Binder.read_output(method_name, status_info_ctype, sandbox_dir)
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

    def call_init(self, *input_idses, code_parameters: str):

        output = self.call_basic_method(*input_idses,
                                        method_role="init",
                                        basic_method=self.wrapper_init_func,
                                        code_parameters=code_parameters )
        return output

    # only input arguments, outputs are returned (as a list if more than 1)
    def call_main(self, *input_idses, code_parameters:str):
        """
        """
        output = self.call_basic_method(*input_idses,
                                        method_role="main",
                                        basic_method=self.wrapper_main_func,
                                        code_parameters=code_parameters)
        return output

    def call_finish(self, *input_idses, code_parameters:str):

        output = self.call_basic_method( *input_idses,
                                         method_role="finalize",
                                         basic_method=self.wrapper_finish_func,
                                         code_parameters=code_parameters)

        return output

    def call_basic_method(self, *input_idses, method_role, basic_method, code_parameters = None):
        """
        """

        method_implementation = self.__get_wrapper_function(method_name)

        if not method_implementation:
            return

        method_description = self.actor.code_description['implementation']['subroutines'][method_role]
        arg_metadata_list = method_description.get('arguments')
        need_code_parameters = method_description.get('need_code_parameters')

        c_arglist = []
        self.__logger.debug( 'RUN MODE: ' + str( self.runtime_settings.run_mode ) )

        ids_ctypes_list = self.__get_ids_ctypes( arg_metadata_list )

        if arg_metadata_list:
            ids_ctypes_list = self.__get_ids_ctypes( arg_metadata_list )

        if input_idses:
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
        if code_parameters and need_code_parameters:
            param_ctype = ParametersCType( code_parameters )
            c_param = param_ctype.convert_to_native_type()
            c_arglist.append( c_param )

        # Add status info to argument list
        status_info_ctype = StatusCType()
        c_status_info = status_info_ctype.convert_to_native_type()
        c_arglist += c_status_info

        # call native MAIN method of wrapper
        method_implementation( *c_arglist )

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
        method_implementation = self.__get_wrapper_function( "set_state")

        if not method_implementation:
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
        method_implementation(cref_state, cref_state_size,  cref_code, cref_msg)

        # Checking returned DIAGNOSTIC INFO
        self.__status_check( status_info_ctype )

    def call_get_state(self) -> str:
        method_implementation = self.__get_wrapper_function("get_state")

        if not method_implementation:
            return

        state = None

        c_ptr_state = ctypes.c_char_p()
        cref_state = ctypes.pointer( c_ptr_state )

        # Add status info to argument list
        status_info_ctype = StatusCType()
        cref_code, cref_msg = status_info_ctype.convert_to_native_type()

        # call native MAIN method of wrapper
        method_implementation( cref_state, cref_code, cref_msg )

        # Checking returned DIAGNOSTIC INFO
        status_info_ctype.convert_to_actor_type( cref_code, cref_msg )
        self.__status_check( status_info_ctype )

        state_raw = cref_state.contents.value
        if state_raw:
            state = state_raw.decode('utf-8','replace')

        return state

    def call_get_timestamp(self) -> float:
        method_implementation = self.__get_wrapper_function("get_timestamp")

        if not method_implementation:
            return

        c_double_timestamp = ctypes.c_double( 0.0 )
        cref_timestamp = ctypes.pointer( c_double_timestamp )

        # Add status info to argument list
        status_info_ctype = StatusCType()
        cref_code, cref_msg = status_info_ctype.convert_to_native_type()

        # call native MAIN method of wrapper
        method_implementation( cref_timestamp, cref_code, cref_msg )

        # Checking returned DIAGNOSTIC INFO
        status_info_ctype.convert_to_actor_type( cref_code, cref_msg )
        self.__status_check( status_info_ctype )

        timestamp = cref_timestamp.contents.value

        return timestamp

