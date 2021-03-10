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

    def __str__(self):
        str_ = 'Name : ' + self.name + '\n' \
               + 'Type : ' + self.type + '\n' \
               + 'Sub-type : ' + self.sub_type + '\n' \
               + 'Intent : ' + self.intent + '\n'
        return str_


class CodeDescription( Dictionarizable ):
    class Arguments( Dictionarizable ):

        def __init__(self):
            self.input: List[Argument] = []
            self.output: List[Argument] = []

        def from_dict(self, dictionary: dict):
            super().from_dict(dictionary)
            self.input = []
            for item in dictionary['input']:
                arg = Argument( item )
                arg.intent = Argument.IN
                self.input.append(arg)

            self.output = []
            for item in dictionary['output']:
                arg = Argument( item )
                arg.intent = Argument.OUT
                self.input.append( arg )



    # type hints
    arguments: Arguments

    def __init__(self):
        self.programming_language: str = None
        self.code_name: str = None
        self.data_type: str = None
        self.arguments = self.Arguments()
        self.code_path: str = None
        self.documentation: str = None
        self.language_specific: dict = None


class Description( Dictionarizable ):

    def __init__(self):
        self.version = None
        self.name = None
        self.code_description = CodeDescription()

    def save(self, stream):
        obj = self.to_dict()
        print( obj )
        yaml.dump( obj, stream=stream, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True )
        pass

    def load(self, stream):
        dict = yaml.load( stream, Loader=yaml.Loader )
        self.from_dict( dict )
        pass
