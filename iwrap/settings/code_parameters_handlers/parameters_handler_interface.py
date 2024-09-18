from abc import ABC, abstractmethod
from typing import Set

class ParametersHandlerInterface(ABC):
    '''
    Interface for parameters handlers.
    All interface methods must be implemented for ParametersHandler to work with iWrap.
    '''
    def __init__(self):
        ...

    @property
    @abstractmethod
    def schema(self):
        '''
        Returns parameters schema as a string.
        '''
        ...

    @property
    @abstractmethod
    def parameters(self):
        '''
        Returns parameters schema as a string.
        eg.:
        <parameters>

            <multiplication_factor>1.5</multiplication_factor>

        </parameters>
        '''
        ...

    @property
    @abstractmethod
    def parameters_path(self):
        '''
        Returns path to current parameters file.
        '''
        ...

    @parameters_path.setter
    @abstractmethod
    def parameters_path(self, path: str) -> None:
        '''
        Sets path to current parameters file.
        Warning: path can be set to file of the same format only.
        Setting path to other format will trigger parsing error.
        '''
        ...

    @property
    @abstractmethod
    def formats(self) -> Set[str]:
        '''
        Returns set of formats supported by implemented handler.
        This set is used to match format specified in YAML description with appropiate handler.
        More than one format can be supported by one handler, eg. XML handler supports: {"legacy-xml", "xml"}
        '''
        ...

    @abstractmethod
    def initialize(self, default_parameters_path: str, schema_path: str):
        '''
        Initializes handler with parameters and schema paths.
        '''
        ...

    @abstractmethod
    def get_parameter(self, path_to_node: str) -> str:
        '''
        Returns parameter value pointed by path_to_node.
        example path to node: "parameters/multiplication_factor"
        '''
        ...

    @abstractmethod
    def set_parameter(self, path_to_node: str, value) -> None:
        '''
        Method used to set parameter value pointed by path_to_node.
        Value can be any type castable to string.
        example path to node: "parameters/multiplication_factor"
        '''
        ...

    @abstractmethod
    def validate(self):
        '''
        Method used to validate parameters.
        It's implementation depends of used parameters type
        eg. XML validates against XSD, JSON against JSONSCHEMA
        '''
        ...

    @abstractmethod
    def restore_default_parameters_path(self):
        '''
        Restores parameters path to original one.
        '''
        ...