import os

import logging
from typing import Any, Dict
from pathlib import Path

import yaml

from iwrap.common.misc import Dictionarizable
from iwrap.settings.code_description import CodeDescription
from iwrap.generation_engine.engine import Engine


class ActorDescription(Dictionarizable ):


    _yaml_tag = u'!actor_description'
    _logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        self.actor_name: str = ''
        self.data_type: str = ''
        self.actor_type: str  = ''
        self.install_dir: str = ''

        self.install_dir = str(Path( Path.home(), 'IWRAP_ACTORS' ))   # TODO: Read install dir from platform settings

        yaml.add_representer(self.__class__, representer=ActorDescription.representer)
        yaml.add_constructor( self._yaml_tag, self.constructor )

    def validate(self, engine: Engine, project_root_dir) -> None:

        # actor_name
        if not self.actor_name:
            raise ValueError( 'Actor name is not set!' )

        # actor_type
        if not self.actor_type:
            self.actor_type = engine.active_generator.name
            self._logger.warning( f'Type of the actor to be generated is not set! Using default one: "{self.actor_type}".' )
        else:
            engine.validate_actor_type( self.actor_type )

        # data_type
        if not self.data_type:
            self.data_type = engine.active_generator.actor_data_types[0]
            ActorDescription._logger.warning(
            f'Data type handled by actor is not set!! Using default one: "{self.data_type}".' )
        else:
            engine.validate_actor_data_type(self.data_type)

        # install_dir
        if not self.install_dir:
            raise ValueError( 'Actor installation directory is not set!' )

        try:
            Path(self.install_dir).mkdir(parents=True, exist_ok=True)
        except Exception as exc:
            raise ValueError('Installation directory path is incorrect or dir cannot be created ["' +self.install_dir + "]" + exc)

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
            ActorDescription._yaml_tag,
            data.to_dict())

    @staticmethod
    def constructor(loader, value):
        data_dict = loader.construct_mapping( value, deep=True )
        obj = ActorDescription()
        obj.from_dict(data_dict)
        return obj
