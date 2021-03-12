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

    def __init__(self):
        self.parameters: str
        self.schema: str

    def from_dict(self, dictionary: dict):
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

    def save(self, stream):
        obj = self.to_dict()
        print( obj )
        yaml.dump( obj, stream=stream, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True )
        pass

    def load(self, stream):
        dict = yaml.load( stream, Loader=yaml.Loader )
        self.from_dict( dict )
        pass
