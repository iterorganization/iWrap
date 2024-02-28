import logging
from pathlib import Path

from jpype import JClass

from .data_storages.data_descriptions import IDSDescription


class JavaIDSDescription(IDSDescription):

    def __init__(self, ids_description:IDSDescription):
        self.ids_description_class = JClass( 'IDSDescription' )
        self.ids_type = ids_description.ids_type
        self.shot = ids_description.shot
        self.run = ids_description.run
        self.occurrence = ids_description.occurrence
        self.idx = ids_description.idx
        self.database = ids_description.database
        self.user = ids_description.user
        self.version = ids_description.version

    def convert_to_native_type(self):
        java_ids_description = self.ids_description_class()

        java_ids_description.ids_type = self.ids_type
        java_ids_description.shot = self.shot
        java_ids_description.run = self.run
        java_ids_description.occurrence = self.occurrence
        java_ids_description.idx= self.idx
        java_ids_description.database = self.database
        java_ids_description.user = self.user
        java_ids_description.version = self.version

        return java_ids_description


# # # # # # # #
class JavaCodeStatus( ):
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
        return self._code, self._message

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
class JavaCodeParameters(  ):
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

    def __init__(self, code_parameters: str):
        self.params = code_parameters

    def convert_to_native_type(self):
        return self.params

    def save(self, sandbox_dir):

        if not self.params:
            return

        file_path = Path(sandbox_dir, 'code_parameters.xml')
        with open( file_path, "wt" ) as file:
            file.write( self.params )

