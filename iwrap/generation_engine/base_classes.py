from abc import ABC, abstractmethod
from typing import Set


class ActorGenerator( ):

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
    def init(self):
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
