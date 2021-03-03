from abc import ABC
from typing import List


class Dictionarizable( ABC ):

    def from_dict(self, dictionary: dict):
        for name, value in dictionary.items():
            if hasattr( self, name ):
                attr = getattr( self, name )
                if isinstance( attr, Dictionarizable ):
                    attr.from_dict( value )
                else:
                    setattr( self, name, value )

    def to_dict(self):
        dict_ = vars( self )
        for key, value in dict_.items():
            if isinstance( value, Dictionarizable ):
                dict_[key] = value.to_dict()
            if isinstance( value, List ):
                dict_[key] = [item.to_dict() for item in value if isinstance( item, Dictionarizable )]

        return dict_

