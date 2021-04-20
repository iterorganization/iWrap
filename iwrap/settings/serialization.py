from abc import ABC, abstractmethod

import yaml


class IWrapSerializer(ABC):

    def __init__(self, stream):
        self.stream = stream

    @abstractmethod
    def save(self, dictionary: dict, stream):
        """Stores code description in a file

        Args:
            serializer (IWrapSerializer): an object responsible for storing dictionary to file of given format
        """
        ...

    @abstractmethod
    def load(self, stream) -> dict:
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """
        ...


class YAMLSerializer(IWrapSerializer):

    def __init__(self, stream):
        super().__init__(stream)

    def save(self, dictionary:dict):
        """Stores code description in a file

        Args:
            serializer (IWrapSerializer): an object responsible for storing dictionary to file of given format
        """
        yaml.dump( dictionary, stream=self.stream, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True )

    def load(self) -> dict:
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """
        dictionary : dict = yaml.load( self.stream, Loader=yaml.Loader )
        return dictionary


