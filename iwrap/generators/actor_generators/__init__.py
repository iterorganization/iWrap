import logging
from abc import abstractmethod
from typing import Set, List

from iwrap.generation_engine import utils
from iwrap.generators import AbstractGenerator


class ActorGenerator(AbstractGenerator):
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


class ActorGeneratorRegistry:
    __builtin_pkg_name: str = 'iwrap.generators.actor_generators'
    __plugin_pkg_name: str = 'iwrap_actor_generator'
    __generators: List[ActorGenerator] = []

    @classmethod
    def initialize(cls):
        cls.__generators  = utils.discover_generators(cls.__builtin_pkg_name, cls.__plugin_pkg_name, ActorGenerator)

        # raises exception if no generator was found
        if len( cls.__generators ) < 1:
            raise RuntimeError( 'ERROR! No valid actor generator can be found!' )

    @classmethod
    def generators(cls): # TODO set as a class property (available since Python 3.9)
        return cls.__generators

    @classmethod
    def get_generator(cls, generator_name) -> ActorGenerator:

        for generator in cls.__generators:
            if generator.name == generator_name:
                return generator

        types = [generator.name for generator in cls.__generators]
        raise ValueError(f'ERROR: No registered generator for provided actor type <{generator_name}>! Registered generators: "{types}".' )