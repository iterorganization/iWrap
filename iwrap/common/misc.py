from abc import ABC, abstractmethod
from typing import List


class Dictionarizable( ABC ):

    @abstractmethod
    def from_dict(self, dictionary: dict):
        for name, value in dictionary.items():
            if not hasattr( self, name ):
                error_msg = f'ERROR: Unknown field "{name}". Configuration seems to be not a valid iWrap file!'
                raise ValueError(error_msg)
            attr = getattr( self, name )
            if isinstance( attr, Dictionarizable ):
                attr.from_dict( value )
            else:
                setattr( self, name, value )
    @abstractmethod
    def to_dict(self):
        dict_ = vars( self )
        for key, value in dict_.items():
            if isinstance( value, Dictionarizable ):
                dict_[key] = value.to_dict()
            elif isinstance( value, List ):
                dict_[key] = [item.to_dict() if isinstance( item, Dictionarizable )  else item for item in value ]

        return dict_

