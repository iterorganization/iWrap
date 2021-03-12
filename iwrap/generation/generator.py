from abc import ABC, abstractmethod

from iwrap.settings.code_description import CodeDescription


class AbstractGenerator(ABC):


    @abstractmethod
    def generate(self, actor_settings: dict, code_description:CodeDescription):
        ...

    @property
    @abstractmethod
    def code_signature(self) -> str:
        ...

