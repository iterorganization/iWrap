import logging
import ctypes

from ..code_parameters import CodeParameters


# # # # # # # #
class StatusCType( ctypes.Structure ):
    '''IDSRef reference structure'''
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    _fields_ = (("_code", ctypes.c_int),
                ("_message", ctypes.c_char_p),
                )
    def __init__(self):
        pass

    @property
    def code(self):
        return self._code

    @code.setter
    def code(self, code):
        self._code = ctypes.c_int( code )

    @property
    def message(self):
        if self._message is None:
            return ''
        return self._message.decode(errors='replace')

    @message.setter
    def message(self, message):
        self._message = ctypes.c_char_p(message.encode('utf-8'))


    def convert_to_native_type(self):
        return ctypes.byref( self )





# # # # # # # #
class ParametersCType( ctypes.Structure ):
    '''IDSRef reference structure'''
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    _fields_ = (("params_", ctypes.c_char_p),
                ("params_size_", ctypes.c_int),
                )

    @property
    def params(self):
        return self.params_

    @params.setter
    def params(self, params):
        self.params_ = ctypes.c_char_p(params.encode('utf-8'))
        str_size = len( self.params_ )
        self.params_size_ =  ctypes.c_int(str_size)

    def convert_to_native_type(self):
        return ctypes.byref( self )


    def __init__(self, codeparams: CodeParameters):
        self.params = codeparams.parameters
