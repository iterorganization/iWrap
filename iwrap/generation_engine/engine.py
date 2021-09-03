import sys
from typing import Set, List

import imas

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generation_engine.generators_mgmt import GeneratorRegistry
from iwrap.settings.platform.platform_settings import PlatformSettings

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


    @property # TODO set as a class property (available since Python 3.9)
    def active_generator(self):
        return Engine._active_generator

    @active_generator.setter # TODO set as a class property (available since Python 3.9)
    def active_generator(self, value):
        if Engine._active_generator == value:
            return

        if isinstance(value, str):
            value = Engine._registry.get_generator(value)

        Engine._active_generator = value
        Engine._active_generator.initialize()

    @property # TODO set as a class property (available since Python 3.9)
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
        from iwrap.settings.project import ProjectSettings
        try:
            Engine._active_generator.configure(info_output_stream = info_output_stream)
            text_decoration = 20 * "="
            print( text_decoration, 'VALIDATING AN ACTOR DESCRIPTION', text_decoration, file=info_output_stream )
            ProjectSettings.get_settings().validate(self)
            print(text_decoration, 'GENERATING AN ACTOR', text_decoration, file=info_output_stream)
            Engine._active_generator.generate( )
            print(text_decoration, 'BUILDING AN ACTOR', text_decoration, file=info_output_stream )
            Engine._active_generator.build()
            print(text_decoration, 'GENERATION COMPLETE!', text_decoration,file=info_output_stream)
            return 0
        except Exception as exc:
            print( 'GENERATION FAILED!', file=info_output_stream )
            print( exc, file=info_output_stream )
            import traceback
            traceback.print_tb( exc.__traceback__ )
            return 1

    @classmethod
    def validate_actor_type(cls, actor_type):
        available_actor_types = [generator.name for generator in Engine._registry.registered_generators]
        if actor_type not in available_actor_types:
            raise ValueError( f'Unknown type of data handled by an actor : "{actor_type}"! Available types: {available_actor_types}.' )

    @classmethod
    def validate_actor_data_type(cls, data_type):
        available_actor_data_types = Engine._active_generator.actor_data_types
        if data_type not in available_actor_data_types:
            raise ValueError( f'Unknown type of data handled by an actor : "{data_type}"! Available types: {available_actor_data_types}.' )

    @classmethod
    def validate_code_data_type(cls, data_type):
        available_code_data_types = Engine._active_generator.code_data_types
        if data_type not in available_code_data_types:
            raise ValueError(
                f'Unknown type of data handled by an actor : "{data_type}"! Available types: {available_code_data_types}.' )

    @classmethod
    def get_ids_types(cls, data_type):

        # first check if data type is valid
        Engine.validate_actor_data_type(data_type)

        # TODO: To add "get_ids_types" to generator ABS (?)
        if data_type == 'legacy':
            ids_list = [ids.value for ids in list( imas.IDSName )]  # pylint: disable=no-member

        return ids_list

    @classmethod
    def validate_programming_language(cls, language):
        available_languages = [lang.lower() for lang in Engine._active_generator.code_languages]
        if language not in available_languages:
            raise ValueError(
                f'Unknown programming language: "{language}"! Available languages: {available_languages}.' )