import os

from pathlib import Path
from typing import Any, Dict

from iwrap.common.misc import Dictionarizable
from iwrap.settings.code_description import CodeDescription
from iwrap.settings.serialization import IWrapSerializer


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
        self.actor_name: str = ''
        self.data_type: str = ''
        self.actor_type: str  = ''
        self.root_dir = os.getcwd()
        self.install_dir = Path(Path.home(), '/IWRAP_ACTORS') # TODO: Read install dir from platform settings
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
        self.actor_name = ''
        self.data_type = ''
        self.actor_type = ''
        self.install_dir = Path(Path.home(), '/IWRAP_ACTORS') # TODO: Read install dir from platform settings
        self.root_dir = os.getcwd()
        self.code_description.clear()

    def save(self, serializer: IWrapSerializer):
        """Stores code description in a file

        Args:
            serializer (IWrapSerializer): an object responsible for storing dictionary to file of given format
        """
        dictionary = self.to_dict()
        serializer.save( dictionary )

    def load(self, serializer: IWrapSerializer):
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dictionary = serializer.load()
        self.from_dict( dictionary )
