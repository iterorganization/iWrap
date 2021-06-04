import ctypes
import os
import logging

import imas

from ..definitions import Argument
from ..code_parameters import CodeParameters


from .data_type import LegacyIDS
from .data_c_binding import ParametersCType, StatusCType


class FortranBinder:

    def __init__(self, actor_name, code_name):
        self.logger = logging.getLogger( 'binding' )
        self.logger.setLevel( logging.DEBUG )

        self.actor_name = actor_name
        self.code_name = code_name

    
    def save_data(self, ids):
        pass
    
    def read_data(self, ids):
        pass
    
    def initialize(self, arguments, codeparams: CodeParameters):
        self.code_parameters = codeparams
        self.formal_arguments = arguments

        self.work_db = self.__create_work_db()
        self.wrapper_func = self.__get_wrapper_function(self.actor_name, self.code_name)

    @staticmethod
    def __create_work_db():
        db_entry = imas.DBEntry( imas.imasdef.MEMORY_BACKEND, 'tmp', 11, 22 )
        db_entry.create()
        return db_entry

    @staticmethod
    def __get_wrapper_function(actor_name, code_name):

        script_path = os.path.dirname( os.path.realpath( __file__ ) )
        lib_path = script_path + '/../../fortran_wrapper/lib/lib' + actor_name + '.so'

        wrapper_lib = ctypes.CDLL( lib_path )
        wrapper_fun = getattr(wrapper_lib,  code_name + 'ual')
        return wrapper_fun

    def __status_check(self, status_info):

        if status_info.code < 0:
            raise Exception(
                "Actor *** '" + self.actor_name + "' *** returned an error (" + str( status_info.code ) + "): '"
                + status_info.message + "'" )

        if status_info.code > 0:
            self.logger.warning( "Actor * '" + self.actor_name + "' * returned diagnostic info: \n     Output flag:      ",
                                       status_info.code, "\n     Diagnostic info: ", status_info.message )


    # only input arguments, outputs are returned (as a list if more than 1)
    def call_native_code(self, *input_idses) :
        """
        """
        input_idses = list(input_idses)

        # their ordering
        full_arguments_list = []

        # LOOP over full_arguments_list
        for formal_arg in self.formal_arguments:
            ids_value = None
            if formal_arg.intent == Argument.IN:
                ids_value = input_idses.pop(0)

            arg = LegacyIDS( self.work_db, formal_arg, ids_value)
            full_arguments_list.append( arg )
            pass

    
        # check conflicting occurences and store data
        arglist = [arg.convert_to_native_type() for arg in full_arguments_list]

        # XML Code Params
        param_c = ParametersCType(self.code_parameters).convert_to_native_type()
                
        # DIAGNOSTIC INFO
        status_info = StatusCType()

        # call the NATIVE function

        self.wrapper_func(*arglist, param_c, status_info.convert_to_native_type())
    
    
        # Checking returned DIAGNOSTIC INFO
        self.__status_check(status_info)
    
        # end DIAGNOSTIC INFO

        # get output data
        results = []
        for arg in full_arguments_list:
            if arg.intent == Argument.OUT:
                results.append(arg.convert_to_actor_type())
    

    
        # final output
        if not results:
            return None
        elif len(results) == 1:
            return results[0]
        else:
            return tuple(results)
