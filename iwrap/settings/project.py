import os

from pathlib import Path
from typing import Any, Dict

import yaml


from iwrap.common.misc import Dictionarizable
from iwrap.settings import SettingsBaseClass
from iwrap.settings.actor_description import ActorDescription
from iwrap.settings.code_description import CodeDescription

#from iwrap.generation_engine.engine import Engine

class ProjectSettings( SettingsBaseClass ):
    """Data class describing iWrap project settings.

    Attributes:

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

        self.actor_description = ActorDescription()
        self.code_description = CodeDescription()

    """Checks settings, field by field, if they are valid to be used to generate actor
    
       Args:
           engine (Engine): generation engine used to validate particular settings
       """
    #def validate(self, engine: Engine) -> None:
    def validate(self, engine ) -> None:

        project_root_dir = self.root_dir

        if not self.code_description:
            raise ValueError( 'Code description structure cannot be empty!')

        self.code_description.validate(engine, project_root_dir)

        if not self.actor_description:
            raise ValueError( 'Actor description structure cannot be empty!' )

        self.actor_description.validate(engine, project_root_dir)

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




    def load(self, file):
        """Loads code description from a file

        Args:
            file: an object responsible for reading dictionary from file of given format
        """
        self.clear()
        objects_read = yaml.load_all( file, Loader=yaml.Loader )

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

        file_real_path = os.path.realpath( file.name )
        self.root_dir = os.path.dirname( file_real_path )


