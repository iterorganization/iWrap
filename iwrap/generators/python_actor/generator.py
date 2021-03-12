from abc import abstractmethod

from iwrap.common.misc import Dictionarizable
from iwrap.generation.generator import AbstractGenerator
from iwrap.settings.code_description import CodeDescription


class Settings(Dictionarizable):

    def __init__(self):
        self.settings = None

class Generator(AbstractGenerator):

    def __init__(self):
        pass

    def generate(self, actor_settings: dict, code_description:CodeDescription):
        pass

    def code_signature(self) -> str:
        return 'To be Defined'