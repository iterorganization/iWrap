import logging
from abc import abstractmethod
from typing import Set, List

from iwrap.generation_engine import utils
from iwrap.generators import AbstractGenerator


class BinderGenerator(AbstractGenerator):
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
    def actor_language(self) -> str:
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


class BinderGeneratorRegistry:
    __builtin_pkg_name: str = 'iwrap.generators.binder_generators'
    __plugin_pkg_name: str = 'iwrap_binder_generator'
    __generators: List[BinderGenerator] = []

    @classmethod
    def initialize(cls):
        cls.__generators  = utils.discover_generators(cls.__builtin_pkg_name, cls.__plugin_pkg_name, BinderGenerator)

        # raises exception if no generator was found
        if len( cls.__generators ) < 1:
            raise RuntimeError( 'ERROR! No valid actor generator can be found!' )

    @classmethod
    def generators(cls): # TODO set as a class property (available since Python 3.9)
        return cls.__generators

    @classmethod
    def get_generator(cls, actor_language: str, code_language: str) -> BinderGenerator:

        for generator in cls.__generators:
            if actor_language == generator.actor_language and code_language in generator.code_languages:
                return generator

        types = [(generator.actor_language, generator.actor_language) for generator in cls.__generators]
        raise ValueError(f'ERROR: No generator found to bind  actor language "{actor_language}" to "{code_language}" '
                         f'! Registered generators: "{types}".' )