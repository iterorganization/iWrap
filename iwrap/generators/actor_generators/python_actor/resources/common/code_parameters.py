import logging
from .code_parameters_handlers.handler_factory import HandlerFactory

class CodeParameters:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    @property
    def schema(self):
        return self.__handler.schema

    @property
    def parameters(self):
        return self.__handler.parameters

    @property
    def parameters_path(self):
        return self.__handler.parameters_path

    @parameters_path.setter
    def parameters_path(self, path: str) -> None:
        self.__handler.parameters_path = path

    @property
    def format(self):
        return self._parameters_format

    def initialize(self):
        self.__handler.initialize(self._default_parameters_path, self._schema_path)

    def get_parameter(self, path_to_node:str) -> str:
        return self.__handler.get_parameter(path_to_node)

    def set_parameter(self, path_to_node:str, value) -> None:
        self.__handler.set_parameter(path_to_node, value)

    def __init__(self, default_parameters_path:str, schema_path:str, parameters_format="xml"):
        self._default_parameters_path = default_parameters_path
        self._schema_path = schema_path

        self._parameters_format = parameters_format.lower()
        self.__handler = HandlerFactory.get_handler(parameters_format)
        self.__handler.initialize(default_parameters_path, schema_path)

    def validate(self):
        self.__handler.validate()
