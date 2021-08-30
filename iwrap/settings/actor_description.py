import os

from typing import Any, Dict

import yaml

from iwrap.common.misc import Dictionarizable
from iwrap.settings.code_description import CodeDescription


class ActorDescription(Dictionarizable ):
    yaml_tag =  u'!actor_description'

    def __init__(self):
        self.actor_name: str = ''
        self.data_type: str = ''
        self.actor_type: str  = ''
        self.install_dir: str = ''

        yaml.add_representer(self.__class__, representer=ActorDescription.representer)
        yaml.add_constructor( self.yaml_tag, self.constructor)

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str, Any]): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.actor_name = ''
        self.data_type = ''
        self.actor_type = ''
        self.install_dir = ''



    @staticmethod
    def representer( dumper, data):
    # ...
        return dumper.represent_mapping(
            ActorDescription.yaml_tag,
            data.to_dict())

    @staticmethod
    def constructor(loader, value):
        data_dict = loader.construct_mapping( value, deep=True )
        obj = ActorDescription()
        obj.from_dict(data_dict)
        return obj
