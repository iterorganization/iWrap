import logging
import sys
from abc import ABC, abstractmethod
from typing import Set


class ActorGenerator( ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __str__(self):
        return self.name

    @property
    @abstractmethod
    def name(self) -> str:
        ...

    @property
    @abstractmethod
    def description(self) -> str:
        ...

    @property
    @abstractmethod
    def actor_data_types(self) -> Set[str]:
        ...

    @property
    @abstractmethod
    def code_data_types(self) -> Set[str]:
        ...

    @property
    @abstractmethod
    def code_languages(self) -> Set[str]:
        ...

    @abstractmethod
    def initialize(self):
        ...

    @abstractmethod
    def configure(self, info_output_stream=sys.stdout):
        ...

    @abstractmethod
    def generate(self):
        ...

    @abstractmethod
    def build(self):
        ...

    @abstractmethod
    def install(self):
        ...

    @abstractmethod
    def cleanup(self):
        ...

    @abstractmethod
    def get_code_signature(self) -> str:
        ...
