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
    IN = 'IN'  # input type of argument
    OUT = 'OUT'  # output type of an argument

class Argument( SettingsBaseClass ):
    """The data class containing information about argument of the native code

    Attributes:
        name (`str`): user name of the argument
        type (`str`): type of the IDS (e.g. 'equilibrium')
        intent : determines if argument is IN or OUT
    """


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


class CodeParameters(SettingsBaseClass):
    """The data class containing information about files defining code parameters.

    Attributes:
        parameters (str): Path to a XML file with code parameters
        schema (str): Path to a XSD file with schema definition for code parameters file
    """

    def __init__(self):
        #: A path to XML file containing native code parameters
        self.parameters: str = ''

        #: A path to XSD file containing schema that allows to validate code parameters XML description
        self.schema: str = ''

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        if self.parameters and not self.schema:
            raise ValueError( 'XSD schema must be set if XML parameters file is specified!' )

        # parameters
        if self.parameters and not Path(project_root_dir, self.parameters).exists():
            raise ValueError( 'Path to XML parameters file is invalid!' )

        # schema
        if self.schema and not Path(project_root_dir, self.schema).exists():
            raise ValueError( 'Path to XSD schema file is invalid!' )

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
    _yaml_tag = u'!code_description'

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
            if  self._language_specific and isinstance(self._language_specific, dict):
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
        self._programming_language: str = ''
        self.code_name: str = None
        self.data_type: str = None
        self._arguments: List[Argument] = []
        self.code_path: str = None
        self.code_parameters: CodeParameters = CodeParameters()
        self.documentation: str = None
        self.language_specific: dict = {}
        yaml.add_representer(self.__class__, representer=CodeDescription.representer)
        yaml.add_constructor( self._yaml_tag, self.constructor )

    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:

        # programming_language
        if not self.programming_language:
            raise ValueError( 'Programming language is not set!' )
        else:
            engine.validate_programming_language(self.programming_language)


        # code_name
        if not self.code_name:
            raise ValueError( 'Name of the code called by actor is not set!' )

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
        if not Path(project_root_dir, self.code_path).exists():
            raise ValueError( 'Path to native code points to not existing location ["' + self.code_path + '"]' )

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
        self.programming_language = None
        self.code_name = None
        self.data_type = None
        self.arguments = []
        self.code_path = None
        self.code_parameters.clear()
        self.documentation = None
        self.language_specific = {}

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()



    @staticmethod
    def representer( dumper, data):
        code_description_dict = data.to_dict()
        return dumper.represent_mapping(
            CodeDescription._yaml_tag,
            code_description_dict)

    @staticmethod
    def constructor(loader, value):
        data_dict = loader.construct_mapping( value, deep=True )
        obj = CodeDescription()
        obj.from_dict(data_dict)
        return obj

    def save(self, stream):
        """Stores code description in a file

        Args:
            serializer (IWrapSerializer): an object responsible for storing dictionary to file of given format
        """
        yaml.dump( self, stream=stream, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True, explicit_end=True )

    def load(self, file):
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """
        self.clear()
        objects_read = yaml.load_all( file, Loader=yaml.Loader )
        objects_read = list(filter(lambda x : isinstance(x, CodeDescription), objects_read ))

        if len(objects_read) < 1:
            raise Exception("The YAML file being looaded doesn't seem to contain valid description of the native code")

        code_description = objects_read[0]
        self.__dict__ = code_description.__dict__

        file_real_path = os.path.realpath( file.name )
        from iwrap.settings.project import ProjectSettings
        ProjectSettings.get_settings().root_dir = os.path.dirname( file_real_path )
