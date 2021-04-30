from typing import Set

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generators.python_actor import generator


class GeneratorRegistry():
    __registry_instance = None

    def __new__(cls):
        if cls.__registry_instance is None:
            cls.__registry_instance = object.__new__(cls)
        return cls.__registry_instance

    def __init__(self):
        self._registered_generators: Set[ActorGenerator] = set()

    @property
    def registered_generators(self) -> Set[ActorGenerator]:
        return self._registered_generators

    def discover_generators(self) -> None:
        # TODO: write a REAL discovery mechanism
        self._registered_generators = {generator.PythonActorGenerator()}


class GeneratorManager():
    _registry: GeneratorRegistry = GeneratorRegistry()
    _active_generator: ActorGenerator = None

    @classmethod
    def get_code_signature(cls) -> str:
        return cls._active_generator.get_code_signature()

    @classmethod
    def get_active_generator(cls):
        return cls._active_generator

    @classmethod
    def initialize(cls):
        GeneratorManager._registry.discover_generators()
        bla = [generator.name for generator in GeneratorManager._registry.registered_generators]
        return cls._active_generator

    @classmethod
    def init_generator(cls, actor_id, actor_data_type_id):
        GeneratorManager.initialize()
        cls._active_generator = generator.PythonActorGenerator()
        cls._active_generator.init()
        pass
