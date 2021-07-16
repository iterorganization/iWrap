import sys
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
        Engine._active_generator.initialize()

    @property
    def registered_generators(self) -> List[ActorGenerator]:
        return Engine._registry.registered_generators

    @staticmethod
    def get_generator(generator_id):
        generator = Engine._registry.get_generator(generator_id)
        return generator


    def startup(self):
        PlatformSettings().initialize()

        # Initialization of registry of generators
        generators_registry = GeneratorRegistry()
        generators_registry.initialize()

        # set default generator
        registered_generators = generators_registry.registered_generators
        if len(registered_generators) > 0:
            Engine._active_generator = registered_generators[0]
            Engine._active_generator.initialize()

    def generate_actor(self, info_output_stream=sys.stdout):
        try:
            Engine._active_generator.configure(info_output_stream = info_output_stream)
            text_decoration = 20 * "="
            print(text_decoration, 'GENERATING AN ACTOR', text_decoration, file=info_output_stream)
            Engine._active_generator.generate( )
            print(text_decoration, 'BUILDING AN ACTOR', text_decoration, file=info_output_stream )
            Engine._active_generator.build()
            print(text_decoration, 'GENERATION COMPLETE!', text_decoration,file=info_output_stream)
        except Exception as exc:
            print( 'GENERATION FAILED!', file=info_output_stream )
            print( exc, file=info_output_stream )
            import traceback
            traceback.print_tb( exc.__traceback__ )
