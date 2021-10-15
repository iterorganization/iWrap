import logging
import os
from typing import List, Dict, Any
from lxml import etree
import yaml

from pathlib import Path
from enum import Enum


from iwrap.generation_engine.engine import Engine
from iwrap.settings import SettingsBaseClass
from iwrap.settings.language_specific.language_settings_mgmt import LanguageSettingsManager


class Intent( Enum ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

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
    __logger = logging.getLogger(__name__ + "." + __qualname__)



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
        ids_list = Engine.get_ids_types(data_type)
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

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_

class Subroutines(SettingsBaseClass):
    """The data class containing information about subroutines to be called from library provided by developer.

    Attributes:
        init (str): A name of subroutine that could be used to initialise the native code (optional)
        main (str): A name of the main subroutine that will be called from actor (mandatory)
        finish (str): A name of subroutine that could be used to finalise the native code (optional)
    """

    def __init__(self):
        #: A name of subroutine that could be used to initialise the native code (optional)
        self.init: str = ''

        #: A name of the main subroutine that will be called from actor (mandatory)
        self.main: str = ''

        #: A name of subroutine that could be used to finalise the native code (optional)
        self.finish: str = ''

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # validate correctness of XML

        if not self.main:
            raise ValueError( 'A name of the main subroutine must provided!' )


    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.init = ''
        self.main = ''
        self.finish = ''

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict(dictionary)

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class CodeParameters(SettingsBaseClass):
    """The data class containing information about files defining code parameters.

    Attributes:
        parameters (str): Path to a XML file with code parameters
        schema (str): Path to a XSD file with schema definition for code parameters file
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


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
            __path = Path(project_root_dir, self.parameters)
            if not __path.exists():
                raise ValueError( f'Path to XML parameters file is invalid! {str(__path)}' )

        # schema
        if self.schema:
            __path = Path(project_root_dir, self.schema)
            if not __path.exists():
                raise ValueError( f'Path to XSD schema file is invalid! {str(__path)}' )

        # validate correctness of XML
        if self.parameters and self.schema:
            self.validate_xml(self.parameters, self.schema)

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
        super().from_dict(dictionary)

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

    def validate_xml(self, parameters_xml_path: str = None, schema_xsd_path: str = None) -> None:
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
        xmlschema_file = etree.parse(schema_xsd_path)
        xmlschema = etree.XMLSchema(xmlschema_file)

        # Parse XML file:
        xml_file = etree.parse(parameters_xml_path)

        # Perform validation:
        xmlschema.assertValid(xml_file)


class CodeDescription( SettingsBaseClass ):
    """Description of the native code used for wrapping the code within an actor.

    Attributes:
        programming_language (`str`): language of native physics code
        code_name (str): name of user method / subroutine to be called, used also as an actor name (Please note: must be *exactly the same* as name
            of called method / subroutine!)
        data_type (:obj:`str`):  data type handled by the physics code { 'Legacy IDS', 'HDC IDS'}
        arguments (list [:obj:`Arguments`]): list of native code in/out arguments
        code_path  (str):  path to system library (C, CPP) , script (Python), etc, containing the physics code and
            method/subroutine to be run
        code_parameters (:obj:`CodeParameters`): user defined parameters of the native code
        documentation (str): human readable description of the native code
        language_specific (Dict[str, Any]): information specific for a given language of the native code
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    @property
    def arguments(self):
        return self._arguments

    @arguments.setter
    def arguments(self, values: List[Argument]):
        self._arguments = []

        for value in values or []:
           if not isinstance(value, Argument):
               value = Argument(value)
           self._arguments.append(value)

    @property
    def programming_language(self):
        return self._programming_language

    @programming_language.setter
    def programming_language(self, value: str):
        self._programming_language = ''

        if value:
            self._programming_language = value.lower()
            # language specific settings depends on language chosen
            # language was not set while language specific settings were read so they need
            # to be set here 'again' converting from dict to a proper object
            if  self._language_specific is not None and isinstance(self._language_specific, dict):
                self._language_specific = LanguageSettingsManager.get_settings_handler( self._programming_language,
                                                                                        self._language_specific )



    @property
    def language_specific(self):
        return self._language_specific

    @language_specific.setter
    def language_specific(self, values):

        # language specific settings depends on language chosen
        # if language was not set yet, language specific settings will be set in language property handler
        self._language_specific = LanguageSettingsManager.get_settings_handler(self._programming_language, values)
        pass

    def __init__(self):
        self.root_dir = os.getcwd()
        self._programming_language: str = ''
        self.subroutines: Subroutines = Subroutines()
        self.data_type: str = None
        self._arguments: List[Argument] = []
        self.code_path: str = None
        self.code_parameters: CodeParameters = CodeParameters()
        self.documentation: str = None
        self.language_specific: dict = {}

    def validate(self, engine: Engine, _not_used: str, **kwargs) -> None:

        project_root_dir = self.root_dir
        # programming_language
        if not self.programming_language:
            raise ValueError( 'Programming language is not set!' )
        else:
            engine.validate_programming_language(self.programming_language)

        # subroutines
        self.subroutines.validate( engine, project_root_dir )

        # data_type
        if not self.data_type:
            raise ValueError( 'Type of data handled by native code is not set!' )
        else:
            engine.validate_code_data_type(self.data_type)

        # arguments
        for argument in self.arguments or []:
            argument.validate(engine, project_root_dir, **{'data_type' : self.data_type})

        # code path
        if not self.code_path:
            raise ValueError( 'Path to native code is not set!' )

        from iwrap.settings.project import ProjectSettings
        __path = Path(project_root_dir, self.code_path)
        if not __path.exists():
            raise ValueError( 'Path to native code points to not existing location ["' + str(__path) + '"]' )

        # code parameters
        self.code_parameters.validate(engine, project_root_dir)

        # documentation
        if self.documentation and not isinstance(self.documentation, str):
            raise ValueError( 'Documentation must be a string (and it is not)!' )

        if not self.language_specific:
            raise ValueError( 'Language specific data are not set!' )
        elif isinstance(self.language_specific, SettingsBaseClass):
            self.language_specific.validate(engine, project_root_dir) # pylint: disable=no-member

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.root_dir = None
        self.programming_language = None
        self.data_type = None
        self.arguments = []
        self.code_path = None
        self.code_parameters.clear()
        self.documentation = None
        self.language_specific = {}
        self.subroutines.clear()

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

    def save(self, file):
        """Stores code description in a file

        Args:
            file: an object responsible for storing dictionary from file of given format
        """
        code_description_dict = self.to_dict()
        yaml.dump( {'code_description': code_description_dict}, stream=file, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True, explicit_end=True )

    def load(self, file):
        """Loads code description from a file

        Args:
            file: an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dict_read = yaml.load( file, Loader=yaml.Loader )
        code_description_dict = dict_read.get('code_description')

        if not code_description_dict:
            raise Exception("The YAML file being loaded doesn't seem to contain valid description of the native code")

        self.from_dict(code_description_dict)

        file_real_path = os.path.realpath(file.name)
        if not self.root_dir:
            self.root_dir = os.path.dirname(file_real_path)
