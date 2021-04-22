from iwrap.generation.base_classes import ActorGenerator
from iwrap.generators.python_actor import generator


class GeneratorManager():
    _registered_generators = ('iwrap.generators.python_actor.generator')
    _active_generator: ActorGenerator = None

    @classmethod
    def get_code_signature(cls) -> str:
        return cls._active_generator.get_code_signature()

    @classmethod
    def get_active_generator(cls):
        return cls._active_generator

    @classmethod
    def init_generator(cls, actor_id, actor_data_type_id):
        cls._active_generator = generator.PythonActorGenerator()
        cls._active_generator.init()
        pass
