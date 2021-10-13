import logging
import os


class CodeParameters:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self):
        self.parameters_file = None
        self.schema_file = None
        self.schema = None  # file name or string or ... (?)
        self.parameters = None  # file name or string or... (?)

        self._code_parameters_dir = os.path.dirname( os.path.realpath( __file__ ) ) + '/../input/'

    def _read_file(self, file_name):
        file_path = self._code_parameters_dir + file_name
        file = open( file_path, 'r' )
        file_str = file.read()
        file_str = str( file_str ).encode( 'utf-8' )
        file.close()
        return file_str

    def initialize(self):
        self.read()
        self.validate()

    def read(self):
        if not self.parameters_file:
            return

        self.parameters = self._read_file( self.parameters_file )
        self.schema = self._read_file( self.schema_file )

    def validate(self):
        # TBD
        pass
