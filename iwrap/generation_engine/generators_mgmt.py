from typing import Set, List

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generators.python_actor.generator import PythonActorGenerator


class GeneratorRegistry():
    __registry_instance = None

    def __new__(cls):
        if cls.__registry_instance is None:
            cls.__registry_instance = object.__new__( cls )
        return cls.__registry_instance

    def __init__(self):
        self._registered_generators: List[ActorGenerator] = []

    def initialize(self):
        self.discover_generators()

    @property
    def registered_generators(self) -> List[ActorGenerator]:
        return self._registered_generators

    def get_generator(self, generator_name) -> ActorGenerator:

        for generator in self._registered_generators:
            if generator.name == generator_name:
                return generator

        raise RuntimeError(f'ERROR: No registered generator for provided actor type <{generator_name}>!' )

    def discover_generators(self) -> None:
        # TODO: write a REAL discovery mechanism
        self._registered_generators = [PythonActorGenerator()]

        # raises exception if no generator was found
        if len( self._registered_generators ) < 1:
            raise RuntimeError( 'ERROR! No valid generator plug-in can be found!' )

