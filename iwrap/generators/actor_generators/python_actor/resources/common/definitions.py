import logging
from abc import ABC, abstractmethod
from typing import Union, List, Any

from .code_parameters import CodeParameters
from .runtime_settings import RuntimeSettings


class Argument(  ):
    """The data class containing information about the arguments of the code

    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    IN = 'IN' # input type of argument
    OUT = 'OUT' # output type of an argument


class Actor(ABC):

    @abstractmethod
    def initialize(self, runtime_settings: RuntimeSettings = None, code_parameters: CodeParameters = None) -> None:
        ...

    @abstractmethod
    def run(self, *args) -> Union[List[Any], Any]:
        ...

    @abstractmethod
    def finalize(self) -> None:
        ...

    @abstractmethod
    def get_state(self) -> str:
        ...

    @abstractmethod
    def set_state(self, state: str) -> None:
        ...

    @abstractmethod
    def get_timestamp(self) -> float:
        ...
