from typing import List, Dict, Any
from lxml import etree

from iwrap.common.misc import Dictionarizable
from iwrap.settings.serialization import IWrapSerializer


class Argument( Dictionarizable ):
    """The data class containing information about argument of the native code

    Attributes:
        name (`str`): user name of the argument
        type (`str`): type of the IDS (e.g. 'equilibrium')
    """
    IN = 'IN' # input type of argument
    OUT = 'OUT' # output type of an argument

    def __init__(self, dictionary: dict):
        self.name = dictionary['name']
        self.type = dictionary['type']
        self.intent = dictionary['intent']

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


class CodeParameters(Dictionarizable):
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

    def validate(self, parameters_xml_path: str = None, schema_xsd_path: str = None) -> None:
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


class CodeDescription( Dictionarizable ):
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

    def __init__(self):
        self.programming_language: str = None
        self.code_name: str = None
        self.data_type: str = None
        self.arguments: List[Argument] = []
        self.code_path: str = None
        self.code_parameters: CodeParameters = CodeParameters()
        self.documentation: str = None
        self.language_specific: dict = {}

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

    def save(self, serializer: IWrapSerializer):
        """Stores code description in a file

        Args:
            serializer (IWrapSerializer): an object responsible for storing dictionary to file of given format
        """
        dictionary = self.to_dict()
        serializer.save( dictionary )

    def load(self, serializer: IWrapSerializer):
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dictionary = serializer.load()
        self.from_dict( dictionary )
