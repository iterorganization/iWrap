import ctypes
import os
import logging

import imas
from iwrap.settings.code_description import Argument
from physics_ii.parameters import Parameters


from .data_type import LegacyIDS
from .data_c_binding import ParametersCType, StatusCType


class PhysicsIIBinder:
    ACTOR_NAME = 'physics_ii'

    def __init__(self):
        pass
    
    def save_data(self, ids):
        pass
    
    def read_data(self, ids):
        pass
    
    
    def initialize(self, arguments, codeparams: Parameters):
        self.logger = logging.getLogger( 'binding' )
        self.logger.setLevel( logging.DEBUG )

        self.code_parameters = codeparams
        self.formal_arguments = arguments

    @staticmethod
    def __create_work_db():
        db_entry = imas.DBEntry( imas.imasdef.MEMORY_BACKEND, 'tmp', 11, 22 )
        db_entry.create()
        return db_entry

    @staticmethod
    def __get_wrapper_function(actor_name):

        script_path = os.path.dirname( os.path.realpath( __file__ ) )
        lib_path = script_path + '/../../wrapper/lib/lib' + actor_name + '.so'

        wrapper_lib = ctypes.CDLL( lib_path )
        wrapper_fun = getattr(wrapper_lib,  actor_name + 'ual')
        return wrapper_fun


    @staticmethod
    def __status_check(status_info, actor_name):

        if status_info.code < 0:
            raise Exception(
                "Actor *** '" + actor_name + "' *** returned an error (" + str( status_info.code ) + "): '"
                + status_info.message + "'" )

        if status_info.code > 0:
            logger_physics_ii.warning( "Actor * '" + actor_name + "' * returned diagnostic info: \n     Output flag:      ",
                                       status_info.code, "\n     Diagnostic info: ", status_info.message )


    # only input arguments, outputs are returned (as a list if more than 1)
    def call_native_code(self, *input_idses) :
        """
        """
        input_idses = list(input_idses)
        work_db = self.__create_work_db()

        wrapper_func = self.__get_wrapper_function(self.ACTOR_NAME)

        # their ordering
        full_arguments_list = []

        # LOOP over full_arguments_list
        for formal_arg in self.formal_arguments:
            ids_value = None
            if formal_arg.intent == Argument.IN:
                ids_value = input_idses.pop(0)

            arg = LegacyIDS( work_db, formal_arg, ids_value)
            full_arguments_list.append( arg )
            pass

    
        # check conflicting occurences and store data
        arglist = [arg.convert_to_native_type() for arg in full_arguments_list]

        # XML Code Params
        param_c = ParametersCType(self.code_parameters).convert_to_native_type()
                
        # DIAGNOSTIC INFO
        status_info = StatusCType()

        # call the actor function

        wrapper_func(*arglist, param_c, status_info.convert_to_native_type())
    
    
        # Checking returned DIAGNOSTIC INFO
        self.__status_check(status_info, self.ACTOR_NAME)
    
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
