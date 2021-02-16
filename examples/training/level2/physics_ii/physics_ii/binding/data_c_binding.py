import ctypes

from physics_ii.parameters import Parameters




# # # # IDSRef internal class # # # #
class ParametersCType( ctypes.Structure ):
    '''CPORef reference structure'''
    _fields_ = (("params_", ctypes.c_char_p),
                ("params_size_", ctypes.c_int),
                ("def_params_", ctypes.c_char_p),
                ("def_params_size_", ctypes.c_int),
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
    def def_params(self):
        return self.def_params_

    @def_params.setter
    def def_params(self, def_params):
        self.def_params_ = ctypes.c_char_p( def_params )
        str_size = len( def_params )
        self.def_params_size_ =  ctypes.c_int( str_size )

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
        self.def_params = codeparams.default_parameters
        self.schema = codeparams.schema


        pass