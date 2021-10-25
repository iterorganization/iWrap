import logging
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
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

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

    @property
    def root_dir_path(self):
        return str( Path( self.project_dir, self.code_description.root_dir ) )

    def __init__(self):
        self.project_dir = os.getcwd()
        self.project_file = ''
        self.actor_description = ActorDescription()
        self.code_description = CodeDescription()

    """Checks settings, field by field, if they are valid to be used to generate actor
    
       Args:
           engine (Engine): generation engine used to validate particular settings
       """
    #def validate(self, engine: Engine) -> None:
    def validate(self, engine ) -> None:

        if not self.code_description:
            raise ValueError( 'Code description structure cannot be empty!')

        self.code_description.validate(engine, self.root_dir_path )

        if not self.actor_description:
            raise ValueError( 'Actor description structure cannot be empty!' )

        self.actor_description.validate(engine, None)

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str, Any]): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative:str = False, project_root_dir:str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict(resolve_path, make_relative, project_root_dir)

    def clear(self):
        """Clears class content, setting default values of class attributes
        """
        self.project_dir = os.getcwd()
        self.project_file = ''
        self.actor_description.clear()
        self.code_description.clear()

    def save(self, file):
        """Stores code description in a file

        Args:
            file : an object responsible for storing data
        """

        actor_description_dict = self.actor_description.to_dict()
        code_description_dict = self.code_description.to_dict()
        dumped = {'actor_description': actor_description_dict, 'code_description': code_description_dict}

        yaml.dump( dumped, stream=file,  default_flow_style=False, sort_keys=False, indent=4, explicit_start=True, explicit_end=True)

        file_real_path = os.path.realpath( file.name )
        self.project_dir = os.path.dirname( file_real_path )
        self.project_file = file.name

    def load(self, file):
        """Loads code description from a file

        Args:
            file: an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dict_read = yaml.load( file, Loader=yaml.Loader )
        if not dict_read:
            raise Exception( "The file being loaded doesn't seem to be a valid YAML" )

        actor_description_dict = dict_read.get('actor_description')
        if actor_description_dict:
            self.actor_description.from_dict(actor_description_dict)

        code_descritption_dict = dict_read.get('code_description')
        if code_descritption_dict:
            self.code_description.from_dict( code_descritption_dict )
        else:
            # YAML file MUST contain at least code description document
            raise Exception(
                "The YAML file being looaded doesn't seem to contain valid description of the native code" )

        file_real_path = os.path.realpath( file.name )
        self.project_dir = os.path.dirname( file_real_path )
        self.project_file = file.name


