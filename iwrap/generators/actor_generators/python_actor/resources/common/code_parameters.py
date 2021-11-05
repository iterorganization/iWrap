import logging
from pathlib import Path

from lxml import etree


class CodeParameters:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    @property
    def schema(self):
        return self.__schema_str

    @property
    def parameters(self):
        return self.__parameters_str

    def __init__(self, default_parameters_path:str, schema_path:str):

        self.__default_params_dir = Path( Path( __file__ ).parent, '../input/')
        self.__default_parameters_path = default_parameters_path
        self.__schema_path = schema_path

        self.parameters_path = None

        self.__schema_str = None
        self.__parameters_str = None

    def _read_file(self, file_path):
        with open( file_path, mode='rt', encoding='utf-8' ) as file:
            file_str = file.read()

        return file_str

    def initialize(self):
        if not self.__default_parameters_path:
            return

        schema_path = Path( self.__default_params_dir, Path(self.__schema_path))
        self.__schema_str = self._read_file( schema_path )

        if self.parameters_path:
            self.__parameters_str = self._read_file( self.parameters_path )
        else:
            default_parameters_path = Path( self.__default_params_dir, Path( self.__default_parameters_path ) )
            self.__parameters_str = self._read_file( default_parameters_path )

        self.validate()

    def validate(self):
        xml_schema_tree = etree.fromstring( self.__schema_str )
        xml_schema_validator = etree.XMLSchema( xml_schema_tree )

        # Parse XML file:
        xml_tree = etree.fromstring( self.__parameters_str )

        # Perform validation:
        xml_schema_validator.assertValid( xml_tree )


    def save(self, stream ):
        stream.write( " Code Parameters ".center(70, '=') )
        stream.write( "\n" )
        stream.write( 'Length:' )
        stream.write( "\n" )
        if self.__parameters_str:
            stream.write( str(len(self.__parameters_str)) )
        else:
            stream.write( '0\n' )
            return
        stream.write( "\n" )
        stream.write( ' Value: '.center(20, '-') )
        stream.write( "\n" )
        stream.write( self.__parameters_str )
        stream.write( "\n" )


