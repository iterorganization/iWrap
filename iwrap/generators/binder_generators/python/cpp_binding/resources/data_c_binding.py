import logging
import ctypes


# # # # # # # #
class StatusCType( ):
    '''IDSRef reference structure'''
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):
        self._code = 0
        self._message = None

    @property
    def code(self):
        return self._code

    @code.setter
    def code(self, code):
        self._code =  code

    @property
    def message(self):
        if self._message is None:
            return ''
        return self._message

    @message.setter
    def message(self, message):
        self._message = message


    def convert_to_native_type(self):

        c_ptr_code = ctypes.c_int(0)
        cref_code = ctypes.pointer( c_ptr_code )

        c_ptr_msg = ctypes.c_char_p()
        cref_msg = ctypes.pointer( c_ptr_msg )

        return [cref_code, cref_msg]

    def convert_to_actor_type(self, c_ptr_status, c_ptr_msg):

        self._code = c_ptr_status.contents.value

        message_raw = c_ptr_msg.contents
        if message_raw:
            try:
                self._message = message_raw.value.decode('utf-8','replace')
            except ValueError as ve:
                self.__logger.warning('An error while encoding status message' + str(ve))
                self._message = ''


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
class ParametersCType(  ):
    '''IDSRef reference structure'''
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    @property
    def params(self):
        if self.params_ is None:
            return ''
        return self.params_

    @params.setter
    def params(self, params):
        self.params_ = params


    def convert_to_native_type(self):

        encoded_params = None

        if self.params_:
            encoded_params = self.params_.encode('utf-8')

        c_ptr_params = ctypes.c_char_p(encoded_params)
        cref_params = ctypes.byref(c_ptr_params)

        return c_ptr_params

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

        stream.write( str(len(code_description.params.encode('utf-8'))) )
        stream.write( "\n" )
        stream.write( ' Value: '.center(20, '-') )
        stream.write( "\n" )
        stream.write( code_description.params )
        stream.write( "\n" )
