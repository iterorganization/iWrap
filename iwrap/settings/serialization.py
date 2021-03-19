from abc import ABC, abstractmethod

import yaml


class IWrapSerializer(ABC):

    def __init__(self, stream):
        self.stream = stream

    @abstractmethod
    def save(self, dictionary: dict, stream):
        ...

    @abstractmethod
    def load(self, stream) -> dict:
        ...


class YAMLSerializer(IWrapSerializer):

    def __init__(self, stream):
        super().__init__(stream)

    def save(self, dictionary:dict):
        yaml.dump( dictionary, stream=self.stream, default_flow_style=False, sort_keys=False, indent=4, explicit_start=True )

    def load(self) -> dict:
        dictionary : dict = yaml.load( self.stream, Loader=yaml.Loader )
        return dictionary


