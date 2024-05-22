from abc import ABC, abstractmethod
from typing import Set

class ParametersHandlerInterface(ABC):
    def __init__(self):
        ...

    @property
    @abstractmethod
    def schema(self):
        ...

    @property
    @abstractmethod
    def parameters(self):
        ...

    @property
    @abstractmethod
    def parameters_path(self):
        ...

    @parameters_path.setter
    @abstractmethod
    def parameters_path(self, path: str) -> None:
        ...

    @property
    @abstractmethod
    def formats(self) -> Set[str]:
        ...

    @abstractmethod
    def initialize(self, default_parameters_path: str, schema_path: str):
        ...

    @abstractmethod
    def get_parameter(self, path_to_node: str) -> str:
        ...

    @abstractmethod
    def set_parameter(self, path_to_node: str, value) -> None:
        ...

    @abstractmethod
    def validate(self):
        '''
        Method used to validate parameters
        '''
        ...