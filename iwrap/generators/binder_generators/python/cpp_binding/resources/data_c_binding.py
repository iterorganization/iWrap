import logging
import ctypes
from pathlib import Path

from .data_storages.data_descriptions import IDSDescription


class IDSCType( ctypes.Structure, IDSDescription ):

    _fields_ = (("ids_type_", ctypes.c_byte * 132),
                ("shot", ctypes.c_int),
                ("run", ctypes.c_int),
                ("occurrence", ctypes.c_int),
                ("idx", ctypes.c_int),
                ("machine_", ctypes.c_byte * 132),
                ("user_", ctypes.c_byte * 132),
                ("version_", ctypes.c_byte * 132),
                )

    @property
    def ids_type(self):
        return ''.join( (chr( x ) for x in self.ids_type_[:]) ).strip()

    @ids_type.setter
    def ids_type(self, ids_type_):
        self.ids_type_[:] = len( self.ids_type_ ) * [ord( ' ' )]
        self.ids_type_[:len( ids_type_ )] = [ord( x ) for x in ids_type_]

    @property
    def machine(self):
        return ''.join( (chr( x ) for x in self.machine_[:]) ).strip()

    @machine.setter
    def machine(self, machine_):
        self.machine_[:] = len( self.machine_ ) * [ord( ' ' )]
        self.machine_[:len( machine_ )] = [ord( x ) for x in machine_]

    @property
    def user(self):
        return ''.join( (chr( x ) for x in self.user_[:]) ).strip()

    @user.setter
    def user(self, user_):
        self.user_[:] = len( self.user_ ) * [ord( ' ' )]
        self.user_[:len( user_ )] = [ord( x ) for x in user_]

    @property
    def version(self):
        return ''.join( (chr( x ) for x in self.version_[:]) ).strip()

    @version.setter
    def version(self, version_):
        self.version_[:] = len( self.version_ ) * [ord( ' ' )]
        self.version_[:len( version_ )] = [ord( x ) for x in version_]

    def __init__(self, ids_description:IDSDescription):
        self.ids_type = ids_description.ids_type
        self.shot = ids_description.shot
        self.run = ids_description.run
        self.occurrence = ids_description.occurrence
        self.idx = ids_description.idx
        self.database = ids_description.database
        self.user = ids_description.user
        self.version = ids_description.version

    def convert_to_native_type(self):
        return ctypes.byref( self )


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

        return cref_code, cref_msg

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

    def save(self, sandbox_dir):

        if not self.params:
            return

        file_path = Path(sandbox_dir, 'code_parameters.xml')
        with open( file_path, "wt" ) as file:
            file.write( self.params )

