import logging
from abc import abstractmethod
from typing import Set, List

from iwrap.generation_engine import utils
from iwrap.generators import AbstractGenerator


class WrapperGenerator(AbstractGenerator):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __str__(self):
        return self.name

    @property
    @abstractmethod
    def type(self) -> str:
        ...

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
    def code_data_types(self) -> Set[str]:
        ...

    @property
    @abstractmethod
    def code_language(self) -> str:
        ...


class WrapperGeneratorRegistry:
    __builtin_pkg_name: str = 'iwrap.generators.wrapper_generators'
    __plugin_pkg_name: str = 'iwrap_wrapper_generator'
    __generators: List[WrapperGenerator] = []

    @classmethod
    def initialize(cls):
        cls.__generators  = utils.discover_generators(cls.__builtin_pkg_name, cls.__plugin_pkg_name, WrapperGenerator)

        # raises exception if no generator was found
        if len( cls.__generators ) < 1:
            raise RuntimeError( 'ERROR! No valid wrapper generator can be found!' )

    @classmethod
    def generators(cls): # TODO set as a class property (available since Python 3.9)
        return cls.__generators

    @classmethod
    def get_generator(cls, type:str, actor_language: str, code_language: str) -> WrapperGenerator:

        if str(code_language).lower() == 'none':
            return None

        # No wrapper actually needed
        if actor_language.lower() == code_language.lower():
            return None

        for generator in cls.__generators:
            if code_language.lower() == generator.code_language.lower() and type.lower() == generator.type.lower():
                return generator

        types = [generator.code_language for generator in cls.__generators]
        raise ValueError(f'ERROR: No generator found to wrap code language "{code_language}" for type "{type}"! \n'
                         f'Registered generators: "{types}".' )
