from abc import ABC, abstractmethod
from typing import List, Any, Dict


class Dictionarizable( ABC ):

    @abstractmethod
    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """

    @abstractmethod
    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        dict_ = vars( self )
        for key, value in dict_.items():
            if isinstance( value, Dictionarizable ):
                dict_[key] = value.to_dict()
            elif isinstance( value, List ):
                dict_[key] = [item.to_dict() if isinstance( item, Dictionarizable )  else item for item in value ]

        return dict_

