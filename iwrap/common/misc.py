from abc import ABC, abstractmethod
from typing import List


class classproperty( object ):

    def __init__(self, fget):
        self.fget = fget

    def __get__(self, owner_self, owner_cls):
        return self.fget( owner_cls )

class Dictionarizable( ABC ):

    @abstractmethod
    def from_dict(self, dictionary: dict):
        for name, value in dictionary.items():
            if hasattr( self, name ):
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
            if isinstance( value, List ):
                dict_[key] = [item.to_dict() if isinstance( item, Dictionarizable )  else item for item in value ]

        return dict_

