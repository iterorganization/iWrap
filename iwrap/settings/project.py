import os

from pathlib import Path
from typing import Any, Dict

import yaml

from iwrap.common.misc import Dictionarizable
from iwrap.settings.actor_description import ActorDescription
from iwrap.settings.code_description import CodeDescription

class ProjectSettings( Dictionarizable ):
    """Data class describing iWrap project settings.

    Attributes:
        name (:obj:`str`) : name of the actor
        data_type (:obj:`str`) : data type handled by the actor
        actor_type (:obj:`str`) : type of the actor to be generated
        code_description (:obj:`CodeDescription`): description of the native code used for wrapping the code within an actor.
    """
    _settings = None

    @classmethod
    def get_settings(cls):
        """
        A singleton class method returning an object (:obj:`ProjectSettings`)

        Returns
             :obj:`ProjectSettings` : singleton object

        """
        if cls._settings is None:
            cls._settings = ProjectSettings()

        return cls._settings

    def __init__(self):
        self.root_dir = os.getcwd()
        home = Path.home()
        self.install_dir = str(Path( home, 'IWRAP_ACTORS')) # TODO: Read install dir from platform settings

        self.actor_description = ActorDescription()
        self.code_description = CodeDescription()

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
        self.root_dir = os.getcwd()
        self.actor_description.clear()
        self.code_description.clear()

    def save(self, stream):
        """Stores code description in a file

        Args:
            stream : an object responsible for storing data
        """

        dumped = (self.actor_description, self.code_description,)

        yaml.dump_all( dumped, stream=stream,  default_flow_style=False, sort_keys=False, indent=4, explicit_start=True, explicit_end=True)




    def load(self, stream):
        """Loads code description from a file

        Args:
            stream: an object responsible for reading dictionary from file of given format
        """
        self.clear()
        objects_read = yaml.load_all( stream, Loader=yaml.Loader )

        for object in objects_read:
            if isinstance(object, ActorDescription):
                self.actor_description.__dict__ = object.__dict__
            elif isinstance(object, CodeDescription):
                code_description = object
            else:
                # unknown object
                raise Exception(
                    "The YAML file being looaded doesn't seem to contain valid description of the native code" )

        # YAML file MUST contain at least code description document
        if not code_description:
            # YAML must contain documents
            raise Exception(
                "The YAML file being looaded doesn't seem to contain valid description of the native code" )

        self.code_description.__dict__ = object.__dict__



