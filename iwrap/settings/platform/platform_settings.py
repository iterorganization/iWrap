from pathlib import Path
from typing import Dict, Any

from iwrap.common.misc import Dictionarizable



class PlatformSettings(Dictionarizable):
    """ TO DO: Read a proper content from config file
    """
    __class_instance = None

    def __new__(cls):
        if cls.__class_instance is None:
            cls.__class_instance = object.__new__(cls)
        return cls.__class_instance

    def __init__(self):
        self.install_dir = str(Path( Path.home(), 'IWRAP_ACTORS' ))   # TODO: Read install dir from platform settings

    def initialize(self):
        #TODO: Load platform settings from file
        pass

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

    def load(self, serializer):
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """

