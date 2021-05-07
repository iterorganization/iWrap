from typing import Set, List

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generation_engine.generators_mgmt import GeneratorRegistry
from iwrap.settings.platform.platform_settings import PlatformSettings


def startup():

    pass


class Engine:
    __class_instance = None
    _registry: GeneratorRegistry = GeneratorRegistry()
    _active_generator: ActorGenerator = None

    def __new__(cls):
        if cls.__class_instance is None:
            cls.__class_instance = object.__new__(cls)
        return cls.__class_instance

    @classmethod
    def get_code_signature(cls) -> str:
        return cls._active_generator.get_code_signature()

    @property
    def active_generator(self):
        return Engine._active_generator

    @active_generator.setter
    def active_generator(self, value):
        if Engine._active_generator == value:
            return

        if isinstance(value, str):
            value = Engine._registry.get_generator(value)

        Engine._active_generator = value
        Engine._active_generator.init()

    @property
    def registered_generators(self) -> List[ActorGenerator]:
        return Engine._registry.registered_generators

    def startup(self):
        PlatformSettings().initialize()

        # Initialization of registry of generators
        generators_registry = GeneratorRegistry()
        generators_registry.initialize()

        # set default generator
        registered_generators = generators_registry.registered_generators
        if len(registered_generators) > 0:
            Engine._active_generator = registered_generators[0]
            Engine._active_generator.init()

    def generate_actor(self):
        Engine._active_generator.generate()