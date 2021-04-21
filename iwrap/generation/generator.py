from abc import ABC, abstractmethod, ABCMeta
from typing import List


class ActorGenerator( ):

    @classmethod
    @abstractmethod
    def get_handled_actor_data_types(cls) -> List[str]:
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
