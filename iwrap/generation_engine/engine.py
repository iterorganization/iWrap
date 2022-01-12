import logging
import sys
import traceback
from typing import Set, List

import imas

from iwrap.generators.actor_generators import ActorGenerator, ActorGeneratorRegistry
from iwrap.generators.binder_generators import BinderGeneratorRegistry
from iwrap.generators.wrapper_generators import WrapperGeneratorRegistry
from iwrap.settings.platform.platform_settings import PlatformSettings

class Engine:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    __class_instance = None
    _active_generator: ActorGenerator = None

    def __new__(cls):
        if cls.__class_instance is None:
            cls.__class_instance = object.__new__(cls)
        return cls.__class_instance


    @property # TODO set as a class property (available since Python 3.9)
    def active_generator(self):
        return Engine._active_generator

    @active_generator.setter # TODO set as a class property (available since Python 3.9)
    def active_generator(self, value):
        if Engine._active_generator == value:
            return

        if isinstance(value, str):
            value = ActorGeneratorRegistry.get_generator(value)

        Engine._active_generator = value
        Engine._active_generator.initialize()

    @property # TODO set as a class property (available since Python 3.9)
    def registered_generators(self) -> List[ActorGenerator]:
        return ActorGeneratorRegistry.generators()

    @staticmethod
    def get_generator(generator_id):
        generator = ActorGeneratorRegistry.get_generator(generator_id)
        return generator

    def startup(self):
        PlatformSettings().initialize()

        # Initialization of registry of generators
        ActorGeneratorRegistry.initialize()
        BinderGeneratorRegistry.initialize()
        WrapperGeneratorRegistry.initialize()

        # set default generator
        registered_generators = ActorGeneratorRegistry.generators()
        if len(registered_generators) > 0:
            Engine._active_generator = registered_generators[0]
            Engine._active_generator.initialize()

    def generate_actor(self, info_output_stream=sys.stdout):
        from iwrap.settings.project import ProjectSettings
        from iwrap.settings.platform.platform_settings import PlatformSettings

        project_root_dir = ProjectSettings.get_settings().root_dir_path

        platform_settings_dict = PlatformSettings().to_dict( resolve_path=True, project_root_dir=project_root_dir )

        project_settings_dict = ProjectSettings.get_settings().to_dict( resolve_path=True,
                                                                       project_root_dir=project_root_dir )

        project_settings_dict.update({'platform_settings': platform_settings_dict})

        actor_generator = Engine._active_generator
        actor_language = actor_generator.actor_language
        actor_generator.initialize(project_settings_dict)

        code_language = ProjectSettings.get_settings().code_description.implementation.programming_language
        binder_generator = BinderGeneratorRegistry.get_generator(actor_language, code_language)
        binder_generator.initialize(project_settings_dict)
        wrapper_generator = WrapperGeneratorRegistry.get_generator(code_language)
        wrapper_generator.initialize(project_settings_dict)

        generators = Engine._active_generator, binder_generator, wrapper_generator
        text_decoration = 20 * "-"
        print( text_decoration, 'VALIDATING AN ACTOR DESCRIPTION', text_decoration, file=info_output_stream )
        try:
            ProjectSettings.get_settings().validate( self )
        except Exception as exc:
            print( 'VALIDATION FAILED!', file=info_output_stream )
            print( exc, file=info_output_stream )
            traceback.print_tb( exc.__traceback__ )
            return 1

        for generator in generators:
            try:
                generator_name: str = generator.__class__.__name__
                print(f'  {generator_name}  '.center(80, '='), file=info_output_stream)
                generator.configure(info_output_stream = info_output_stream)
                print(text_decoration, 'GENERATING', text_decoration, file=info_output_stream)
                generator.generate(project_settings_dict)
                print(text_decoration, 'BUILDING', text_decoration, file=info_output_stream )
                generator.build(project_settings_dict)
                print(text_decoration, 'GENERATION COMPLETE!', text_decoration, file=info_output_stream)

            except Exception as exc:
                print( 'GENERATION FAILED!', file=info_output_stream )
                print( exc, file=info_output_stream )
                traceback.print_tb( exc.__traceback__ )
                return 1

        print( 'ALL DONE!', file=info_output_stream )
        return 0

    @classmethod
    def validate_actor_type(cls, actor_type):
        available_actor_types = [generator.name for generator in ActorGeneratorRegistry.generators()]
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
