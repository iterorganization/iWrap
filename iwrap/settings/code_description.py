from typing import List

import yaml

from iwrap.common.misc import Dictionarizable


class Argument( Dictionarizable ):
    IN = 'IN'
    OUT = 'OUT'

    def __init__(self, dictionary: dict):
        self.name = dictionary['name']
        self.type = dictionary['type']
        self.sub_type = dictionary['sub_type']

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self):
        super().to_dict()

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Sub-type : ' + self.sub_type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_

class CodeParameters( Dictionarizable ):
    """The data class containing information about files defining code parameters.
    """

    def __init__(self):

        #: A path to XML file containing native code parameters
        self.parameters: str

        #: A path to XSD file containing schema that allows to validate code parameters XML description
        self.schema: str

    def from_dict(self, dictionary: dict) -> None:
        """Method restores object status based on passed dictionary.

           Args:
               dictionary: The parameter that stores values to be used to restore object state.
           """
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()


class Arguments( Dictionarizable ):

    def __init__(self):
        self.input: List[Argument] = []
        self.output: List[Argument] = []

    def from_dict(self, dictionary: dict):
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


    def to_dict(self):
        return super().to_dict()


class CodeDescription( Dictionarizable ):

    def __init__(self):
        self.programming_language: str = None
        self.code_name: str = None
        self.data_type: str = None
        self.arguments : Arguments= Arguments()
        self.code_path: str = None
        self.code_parameters : CodeParameters = CodeParameters()
        self.documentation: str = None
        self.language_specific: dict = None

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()


