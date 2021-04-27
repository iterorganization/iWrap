from pathlib import Path
from typing import Dict, Any

from iwrap.common.misc import Dictionarizable
from iwrap.settings.serialization import IWrapSerializer


class PlatformSettings(Dictionarizable):
    """ TO DO: Read a proper content from config file
    """

    def __init__(self):
        self.installation_dir = Path.home() + '/IWRAP_ACTORS'

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict(dictionary)

    def clear(self):
        """Clears class content, setting default values of class attributes
        """

        self.__init__()

    def to_dict(self) -> None:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        pass

    def load(self, serializer: IWrapSerializer):
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """
        self.clear()
        dictionary = serializer.load()
        self.from_dict(dictionary)
