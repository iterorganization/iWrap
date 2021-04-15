from typing import List, Dict, Any

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


class CodeParameters( Dictionarizable ):
    """The data class containing information about files defining code parameters.
    """

    def __init__(self):
        #: A path to XML file containing native code parameters
        self.parameters: str = ''

        #: A path to XSD file containing schema that allows to validate code parameters XML description
        self.schema: str = ''

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


class Arguments( Dictionarizable ):
    """
        Attributes:
            input (List [:obj:`Argument`]) : list of input arguments
            output (List [:obj:`Argument`]) : list of output arguments
    """

    def __init__(self):
        self.input: List[Argument] = []
        self.output: List[Argument] = []

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
       """

        super().from_dict( dictionary )

        self.input = []
        for item in dictionary['input']:
            arg = Argument( item )
            arg.intent = Argument.IN
            self.input.append( arg )

        self.output = []
        for item in dictionary['output']:
            arg = Argument( item )
            arg.intent = Argument.OUT
            self.input.append( arg )

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class CodeDescription( Dictionarizable ):
    """Description of the native code used for wrapping the code within an actor.

    Attributes:
        programming_language (`str`): language of native physics code
        code_name (str): name of user method / subroutine to be called, used also as an actor name (Please note: must be *exactly the same* as name
            of called method / subroutine!)
        data_type (:obj:`str`):  data type handled by the physics code { 'Legacy IDS', 'HDC IDS'}
        arguments (:obj:`Arguments`): list of native code in/out arguments
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
        self.arguments: Arguments = Arguments()
        self.code_path: str = None
        self.code_parameters: CodeParameters = CodeParameters()
        self.documentation: str = None
        self.language_specific: dict = None

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def clear(self):
        """Clears class content, setting default values of class attributes
        """

        self.__init__()

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
