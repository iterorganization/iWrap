import inspect
from abc import ABC, abstractmethod
from typing import List, Any, Dict


class Dictionarizable( ABC ):

    def _list_attributes(self):
        members_list = inspect.getmembers(self,  lambda member: not inspect.isroutine(member))
        members_list = filter( lambda member: not member[0].startswith( "_" ), members_list )

        return members_list


    @abstractmethod
    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
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
    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        obj_as_dict_ = self._list_attributes()
        dict_ = {}
        for key, value in self._list_attributes():
            if isinstance( value, Dictionarizable ):
                dict_[key] = value.to_dict()
            elif isinstance( value, List ):
                dict_[key] = [item.to_dict() if isinstance( item, Dictionarizable ) else item for item in value]
            else:
                dict_[key] = value

        return dict_
