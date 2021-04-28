import ctypes

from physics_ii.parameters import Parameters




# # # # IDSRef internal class # # # #
class ParametersCType( ctypes.Structure ):
    '''IDSRef reference structure'''
    _fields_ = (("params_", ctypes.c_char_p),
                ("params_size_", ctypes.c_int),
                ("schema_", ctypes.c_char_p),
                ("schema_size_", ctypes.c_int),
                )

    @property
    def params(self):
        return self.params_

    @params.setter
    def params(self, params):
        self.params_ = ctypes.c_char_p(params)
        str_size = len( params )
        self.params_size_ =  ctypes.c_int(str_size)

    @property
    def schema(self):
        return self.schema_

    @schema.setter
    def schema(self, schema):
        self.schema_ = ctypes.c_char_p( schema )
        str_size = len( schema )
        self.schema_size_ = ctypes.c_int( str_size )

    def __init__(self, codeparams: Parameters):
        self.params = codeparams.code_parameters
        self.schema = codeparams.schema
