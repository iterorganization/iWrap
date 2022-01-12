import logging
import ctypes

from iwrap.generators.actor_generators.python_actor.resources.common.code_parameters import CodeParameters


# # # # # # # #
class StatusCType( ctypes.Structure ):
    '''IDSRef reference structure'''
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    _fields_ = (("_code", ctypes.c_int),
                ("_message", ctypes.c_char_p),
                )
    def __init__(self):
        self._code = 0
        self._message = None

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

    def read(self, stream ):
        # read returned code
        ret_code = stream.readline()
        ret_code = int(ret_code)
        self.code = ret_code

        # read size of message
        msg_size = stream.readline()
        msg_size = int( msg_size )

        msg = stream.readlines()
        msg = ''.join(msg)
        if msg_size != len(msg):
            raise ValueError('ERROR: Message size differs!')

        self.message = msg.strip()


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
        if self.params_ is None:
            return ''
        return self.params_.decode(errors='replace')

    @params.setter
    def params(self, params):
        if not params:
            self.params_ = None
            self.params_size_ = 0
            return

        self.params_ = ctypes.c_char_p(params.encode('utf-8'))
        str_size = len( self.params_ )
        self.params_size_ =  ctypes.c_int(str_size)

    def convert_to_native_type(self):
        return ctypes.byref( self )

    def __init__(self, code_parameters: str):
        self.params = code_parameters

    @classmethod
    def save(cls, code_description, stream ):
        stream.write( " Code Parameters ".center(70, '=') )
        stream.write( "\n" )
        stream.write( 'Length:' )
        stream.write( "\n" )
        if not code_description:
            stream.write( '0\n' )
            return

        if not code_description.params:
            stream.write( '0\n' )
            return

        stream.write( str(len(code_description.params)) )
        stream.write( "\n" )
        stream.write( ' Value: '.center(20, '-') )
        stream.write( "\n" )
        stream.write( code_description.params )
        stream.write( "\n" )
