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

    def save(self, value):
        """Stores code description in a file

        Args:
            serializer (IWrapSerializer): an object responsible for storing dictionary to file of given format
        """
        yaml.dump_all( value, stream=self.stream, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True )

    def load(self) -> dict:
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """

        results = yaml.load_all( self.stream, Loader=yaml.Loader )
        x = [result for result in results]
        #dictionary : dict = yaml.load_all( self.stream, Loader=yaml.Loader )
        return dictionary


