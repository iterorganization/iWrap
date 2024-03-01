import logging
from pathlib import Path
from typing import Any, List

import jpype
from jpype.types import *

from .data_storages import IDSConvertersRegistry

from ..common.definitions import Argument
from ..common.binder import Binder

from .data_storages.ids_converter import LegacyIDSConverter
from .data_j_binding import JavaCodeParameters, JavaCodeStatus, JavaIDSDescription
from ..common.runtime_settings import RuntimeSettings, RunMode, DebugMode
from ..common import exec_system_cmd

class LanguageBinder(Binder):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def standalone_cmd(self, method_name:str) -> str:
        exec = 'java -cp $CLASSPATH:' \
               + self.actor.actor_dir + '/jar/*:' \
               + self.actor.actor_dir + '/wrapper/lib/* Standalone4' + self.actor.name + '_' + method_name

        return exec

    def __init__(self):
        self.ids_converter = None
        self.actor = None

        self.actor_dir = None

        self.runtime_settings: RuntimeSettings = None
        self.arg_metadata_list = None

        self.java_wrapper_class = None

    def initialize(self, actor):
        self.actor = actor

        IDSConvertersRegistry.initialize()
        data_type = self.actor.code_description['implementation']['data_type']
        self.ids_converter = IDSConvertersRegistry.get_converter(data_type, 'java')

        self.runtime_settings = actor.get_runtime_settings()
        self.ids_ctype_list = None
        self.actor_dir = actor.actor_dir

        sandbox_dir = self.actor.sandbox.path

        if not jpype.isJVMStarted():
            jpype.startJVM()

        self.java_wrapper_class = self.__get_wrapper_library()

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



    def __status_check(self, status_info: JavaCodeStatus):

        actor_name = self.actor.name
        if status_info.code < 0:
            raise Exception(
                "Actor *** '" + actor_name + "' *** returned an error (" + str( status_info.code ) + "): '"
                + status_info.message + "'" )

        if status_info.code > 0:
            self.__logger.warning(
                "Actor * '" + actor_name + "' * returned diagnostic info: \n     Output flag:      " +
                str(status_info.code) + "\n     Diagnostic info: " + status_info.message )

    def __get_wrapper_library(self):

        if not jpype.isJVMStarted():
            jpype.startJVM()

        code_file = self.actor.code_description['implementation']['code_path'].split('/')[-1]
        code_path = self.actor_dir + '/wrapper/lib/' + code_file
        print('JAR PATH IS: ', code_path)
        jpype.addClassPath(code_path)

        wrapper_path = self.actor_dir + '/jar/' + self.actor.name + '.jar'
        print('JAR PATH IS: ', wrapper_path)
        jpype.addClassPath(wrapper_path)

        java_fqcn = 'Wrapper4' + self.actor.name
        # TODO Use generator specific 'include_path' handling

        java_class = JClass(java_fqcn)
        return java_class()

    def __get_wrapper_function(self, method_role: str):

        subroutines = self.actor.code_description['implementation']['subroutines']

        if not subroutines.get( method_role ):
            return None

        wrapper_fun = getattr( self.java_wrapper_class, method_role )
        return wrapper_fun


    def __get_ids_native(self, arg_metadata_list):

        ids_list = []

        # LOOP over ids
        for arg_meta_data in arg_metadata_list:
            ids_java = self.ids_converter.prepare_native_type( JavaIDSDescription, arg_meta_data['type'] )
            ids_java.intent = arg_meta_data['intent']
            ids_list.append( ids_java )

        return ids_list

    def run_standalone(self, *input_idses, method_name, arg_metadata_list, code_parameters, exec_command, sandbox_dir:str, output_stream):

        self.__logger.debug( "RUNNING STDL" )
        # if arg_metadata_list:
        # check if a number of provided arguments is correct
        self.__check_inputs(input_idses, arg_metadata_list)

        ids_ctypes_list = self.__get_ids_native( arg_metadata_list )

        tmp_ids_list = list(input_idses)
        for ids_ctype in ids_ctypes_list:
            if ids_ctype.intent == Argument.IN:
                ids_object = tmp_ids_list.pop(0)
                self.ids_converter.convert_to_native_type(ids_ctype, ids_ctype.intent, ids_object)


        param_ctype = JavaCodeParameters(code_parameters) if code_parameters  else  None
        status_info_ctype = JavaCodeStatus()

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

    def call_init(self, *input_idses, code_parameters:str):

        output = self.call_basic_method(*input_idses,
                                        method_role="init",
                                        code_parameters=code_parameters)

        return output

    # only input arguments, outputs are returned (as a list if more than 1)
    def call_main(self, *input_idses, code_parameters:str):
        """
        """
        output = self.call_basic_method(*input_idses,
                                        method_role="main",
                                        code_parameters=code_parameters)
        return output

    def call_finish(self, *input_idses, code_parameters:str):

        output = self.call_basic_method(*input_idses,
                                        method_role="finalize",
                                        code_parameters=code_parameters)

        return output


    def call_basic_method(self, *input_idses, method_role, code_parameters = None):
        """
        """

        method_implementation = self.__get_wrapper_function(method_role)

        if not method_implementation:
            return

        method_description = self.actor.code_description['implementation']['subroutines'][method_role]
        arg_metadata_list = method_description.get('arguments')
        need_code_parameters = method_description.get('need_code_parameters')

        c_arglist = []
        ids_native_list = []
        self.__logger.debug( 'RUN MODE: ' + str( self.runtime_settings.run_mode ) )

        if arg_metadata_list:
            ids_native_list = self.__get_ids_native( arg_metadata_list )

            if input_idses:
            # check if a number of provided arguments is correct
                self.__check_inputs( input_idses, arg_metadata_list )

            tmp_ids_list = list( input_idses )
            for ids_ctype in ids_native_list:
                ids_object = None
                if ids_ctype.intent == Argument.IN:
                    ids_object = tmp_ids_list.pop( 0 )

                c_ids = self.ids_converter.convert_to_native_type( ids_ctype, ids_ctype.intent, ids_object )
                c_arglist.append( c_ids )

        # Code Parameterss
        if code_parameters and need_code_parameters:
            param_ctype = JavaCodeParameters( code_parameters )
            c_param = param_ctype.convert_to_native_type()
            c_arglist.append( c_param )

        # call native MAIN method of wrapper
        method_implementation( *c_arglist )

        # get output data
        results = []
        for ids_ctype in ids_native_list:
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
        method_implementation = self.__get_wrapper_function("set_state")

        if not method_implementation:
            return

        if not state:
            return

        method_implementation(state)



    def call_get_state(self) -> str:
        method_implementation = self.__get_wrapper_function("get_state")

        if not method_implementation:
            return

        state = method_implementation()

        return state

    def call_get_timestamp(self) -> float:
        method_implementation = self.__get_wrapper_function("get_timestamp")

        if not method_implementation:
            return

        timestamp = method_implementation()

        return timestamp


