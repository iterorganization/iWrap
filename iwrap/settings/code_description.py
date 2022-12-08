import logging
import os
from typing import List, Dict, Any
from lxml import etree
import yaml

from pathlib import Path
from enum import Enum

from iwrap.common import utils
from iwrap.generation_engine.engine import Engine
from iwrap.settings import SettingsBaseClass
from iwrap.settings.settings.language_settings_mgmt import LanguageSettingsManager


class Intent( Enum ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    IN = 'IN'  # input type of argument
    OUT = 'OUT'  # output type of an argument


class Argument( SettingsBaseClass ):
    """The data class containing information about argument of the native code

    Attributes:
        name (`str`): user name of the argument
        type (`str`): type of the IDS (e.g. 'equilibrium')
        intent : determines if argument is IN or OUT
    """
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    @property
    def intent(self):
        return self._intent.value

    @intent.setter
    def intent(self, value):
        self._intent = Intent[value.upper()]

    def __init__(self, dictionary: dict):
        self.name = dictionary['name']
        self.type = dictionary['type']
        self._intent = None
        self.intent = dictionary['intent']

    def clear(self):
        # The whole list is cleared, so no need to clear particular elements
        pass

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:
        if not self.name:
            raise ValueError( 'Argument name is not set!' )

        if not self.type:
            raise ValueError( 'Argument IDS type is not set!' )

        data_type = kwargs['data_type']
        ids_list = Engine.get_ids_types( data_type )
        if self.type not in ids_list:
            raise ValueError( f'Incorrect IDS type {self.type} of argument {self.name}!' )

        if not self._intent:
            raise ValueError( 'Argument intent is not set!' )

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
        """

        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict(resolve_path, make_relative, project_root_dir)

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_


class Subroutines( SettingsBaseClass ):
    """The data class containing information about subroutines to be called from library provided by developer.

    Attributes:
        init (str): A name of subroutine that could be used to initialise the native code (optional)
        main (str): A name of the main subroutine that will be called from actor (mandatory)
        finalize (str): A name of subroutine that could be used to finalise the native code (optional)
    """

    def __init__(self):
        # A name of subroutine that could be used to initialise the native code (optional)
        # (Please note: must be *exactly the same* as name of called method / subroutine!)
        self.init: str = ''

        # A name of the main subroutine that will be called from actor (mandatory)
        # (Please note: must be *exactly the same* as name of called method / subroutine!)
        self.main: str = ''

        # A name of subroutine that could be used to finalise the native code (optional)
        # (Please note: must be *exactly the same* as name of called method / subroutine!)
        self.finalize: str = ''

    def validate(self, engine: Engine, project_root_dir: str) -> None:
        # validate correctness of XML

        if not self.main:
            raise ValueError( 'A name of the main subroutine must provided!' )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.init = ''
        self.main = ''
        self.finalize = ''

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict(resolve_path, make_relative, project_root_dir)


class Implementation( SettingsBaseClass ):
    """The data class containing information about user code implementation.

    Attributes:
        root dir (str): root directory
        programming_language (str): language of native physics code
        data_type (:obj:str):  data type handled by the physics code { 'Legacy IDS', 'HDC IDS'}
        code_path  (str):  path to system library (C, CPP) , script (Python), etc, containing the physics code and
            method/subroutine to be run
        include path (str): a module's / header's file path
        code_parameters (:obj:CodeParameters): user defined parameters of the native code
        subroutines (:obj:Subroutines): name of user method / subroutine to be called, used also as an actor name
    """
    @property
    def programming_language(self):
        return self._programming_language

    @programming_language.setter
    def programming_language(self, value: str):
        self._programming_language = ''
        if value:
            self._programming_language = value.lower()
            self._master.change_language_specific()

    def __init__(self, master):
        self.root_dir = '.'
        self._programming_language: str = ''
        self.data_type: str = None
        self.code_path: str = None
        self.include_path = ''
        self._master = master
        self.code_parameters: CodeParameters = CodeParameters()
        self.subroutines: Subroutines = Subroutines()

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:
        # programming_language
        if not self.programming_language:
            raise ValueError( 'Programming language is not set!' )
        else:
            engine.validate_programming_language( self.programming_language )

        # data_type
        if not self.data_type:
            raise ValueError( 'Type of data handled by native code is not set!' )
        else:
            engine.validate_code_data_type( self.data_type )

        # code path
        if not self.code_path:
            raise ValueError( 'Path to native code is not set!' )

        __path = utils.resolve_path( self.code_path, project_root_dir )
        if not Path(__path).exists():
            raise ValueError( 'Path to native code points to not existing location ["' + str( __path ) + '"]' )

        # code parameters
        self.code_parameters.validate( engine, project_root_dir )

        # include path
        if not self.include_path:
            raise ValueError( 'Path to include/module file is not set!' )

        __path = utils.resolve_path( self.include_path, project_root_dir)
        if not Path(__path).exists():
            raise ValueError( f'Path to include/module file is not valid! {str( __path )}' )

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.root_dir = '.'
        self.programming_language = None
        self.data_type = None
        self.code_path = None
        self.subroutines.clear()
        self.code_parameters.clear()

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            # include_path
            __path = utils.resolve_path( self.include_path, project_root_dir )
            ret_dict.update( {'include_path': __path} )

            # code_path
            code_path = self.code_path
            __path = utils.resolve_path( code_path, project_root_dir )
            ret_dict.update( {'code_path': __path} )

        return ret_dict


class CodeParameters( SettingsBaseClass ):
    """The data class containing information about files defining code parameters.

    Attributes:
        parameters (str): Path to a XML file with code parameters
        schema (str): Path to a XSD file with schema definition for code parameters file
    """
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        #: A path to XML file containing native code parameters
        self.parameters: str = ''

        #: A path to XSD file containing schema that allows to validate code parameters XML description
        self.schema: str = ''

    def validate(self, engine: Engine, project_root_dir: str) -> None:
        if self.parameters and not self.schema:
            raise ValueError( 'XSD schema must be set if XML parameters file is specified!' )

        # parameters
        if self.parameters:
            __path = utils.resolve_path( self.parameters, project_root_dir )
            if not Path(__path).exists():
                raise ValueError( f'Path to XML parameters file is invalid! {str( __path )}' )

        # schema
        if self.schema:
            __path = utils.resolve_path( self.schema, project_root_dir )
            if not Path(__path).exists():
                raise ValueError( f'Path to XSD schema file is invalid! {str( __path )}' )

        # validate correctness of XML
        if self.parameters and self.schema:
            self.validate_xml( self.parameters, self.schema, project_root_dir )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.parameters = ''
        self.schema = ''

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative=False, project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            # parameters
            if self.parameters:
                __path = utils.resolve_path( self.parameters, project_root_dir )
                ret_dict.update( {'parameters': __path} )

            # schema
            if self.schema:
                __path = utils.resolve_path( self.schema, project_root_dir )
                ret_dict.update( {'schema': __path} )

        return ret_dict

    def validate_xml(self, parameters_xml_path: str, schema_xsd_path:str, root_dir:str) -> None:
        """Self validation of XML file against given schema file (XSD).

        Args:
            parameters_xml_path (str): The absolute path string to the XML file with parameters data
            schema_xsd_path (str): The absolute path string to the XSD file with schema of the XML file

        Correct file paths for the validation process will cause the method to run without errors.
        If the verification process fails or an error occurs, an exception is thrown,
        and possibly there is a mismatch between the file and its schema
        or stored files or file paths are damaged.
        """
        # In case where no parameters have been provided use class attributes.
        if parameters_xml_path is None or schema_xsd_path is None:
            parameters = self.parameters
            schema = self.schema

        # Parse XSD file:
        schema_xsd_path = utils.resolve_path(schema_xsd_path, root_dir)
        xmlschema_file = etree.parse( schema_xsd_path )
        xmlschema = etree.XMLSchema( xmlschema_file )

        # Parse XML file:
        parameters_xml_path = utils.resolve_path(parameters_xml_path, root_dir)
        xml_file = etree.parse( parameters_xml_path )

        # Perform validation:
        xmlschema.assertValid( xml_file )


class CodeDescription( SettingsBaseClass ):
    """Description of the native code used for wrapping the code within an actor.

    Attributes:
        arguments (list [:obj:`Arguments`]): list of native code in/out arguments
        documentation (str): human readable description of the native code
        settings (dict): native code settings
        implementation(:obj:`Implementation`): native code implementation info
    """
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    @property
    def arguments(self):
        return self._arguments

    @arguments.setter
    def arguments(self, values: List[Argument]):
        self._arguments = []

        for value in values or []:
            if not isinstance( value, Argument ):
                value = Argument( value )
            self._arguments.append( value )

    @property
    def settings(self):
        return self._settings

    @settings.setter
    def settings(self, values):
        # language specific settings depends on language chosen
        # if language was not set yet, language specific settings will be set in language property handler
        self._settings = LanguageSettingsManager.get_settings_handler( self.implementation.programming_language, values )

    def __init__(self):
        self._arguments: List[Argument] = []
        self.implementation: Implementation = Implementation(self)
        self.documentation: str = None
        self._settings: dict = {}

    def change_language_specific(self):
        """ Update settings when programming language changed.
        """
        if self._settings is not None and isinstance(self._settings, dict):
            self._settings = LanguageSettingsManager.get_settings_handler(self.implementation.programming_language,
                                                                                          self._settings)

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:
        # arguments
        for argument in self.arguments or []:
            argument.validate( engine, project_root_dir, **{'data_type': self.implementation.data_type} )

        # implementation
        self.implementation.validate(engine, project_root_dir)

        # documentation
        if self.documentation and not isinstance( self.documentation, str ):
            raise ValueError( 'Documentation must be a string (and it is not)!' )

        # settings
        if isinstance(self.settings, SettingsBaseClass):
            self.settings.validate(engine, project_root_dir)  # pylint: disable=no-member

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.arguments = []
        self.documentation = None
        self.implementation.clear()
        self.settings = {}

    def to_dict(self, resolve_path: bool = False,
                make_relative: bool = False,
                project_root_dir: str = None) -> Dict[ str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        return ret_dict

    def save(self, file):
        """Stores code description in a file

        Args:
            file: an object responsible for storing dictionary from file of given format
        """
        code_description_dict = self.to_dict()
        yaml.dump( {'code_description': code_description_dict}, stream=file, default_flow_style=False, sort_keys=False,
                   indent=4, explicit_start=True, explicit_end=True )

    def load(self, file):
        """Loads code description from a file

        Args:
            file: an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dict_read = yaml.load( file, Loader=yaml.Loader )
        if not dict_read:
            raise Exception( "The file being loaded doesn't seem to be a valid YAML" )

        code_description_dict = dict_read.get( 'code_description' )
        if not code_description_dict:
            raise Exception( "The YAML file being loaded doesn't seem to contain valid description of the native code" )

        self.from_dict( code_description_dict )

        file_real_path = os.path.realpath( file.name )
        if not self.implementation.root_dir:
            self.implementation.root_dir = os.path.dirname( file_real_path )


