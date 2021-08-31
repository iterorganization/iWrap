import pkgutil
from typing import Set, List

from iwrap.generation_engine.base_classes import ActorGenerator



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

        types = [generator.name for generator in self.registered_generators]
        raise ValueError(f'ERROR: No registered generator for provided actor type <{generator_name}>! Registered generators: "{types}".' )

    def __iter_namespace(self, ns_pkg):
        # Specifying the second argument (prefix) to iter_modules makes the
        # returned name an absolute name instead of a relative one. This allows
        # import_module to work without having to do additional modification to
        # the name.
        return pkgutil.iter_modules( ns_pkg.__path__, ns_pkg.__name__ + "." )

    def discover_generators(self) -> None:
        from iwrap.generators.python_actor.generator import PythonActorGenerator

        self._registered_generators = []

        try:
            import iwrap_actor_generator # pylint: disable=import-error
            import sys
            import importlib

            for finder, name, ispkg in self.__iter_namespace( iwrap_actor_generator ):
                importlib.import_module( name )

        except Exception as exc:
            print( 'INFO: No additional generator plug-ins has been found' )

        actor_generators_list =  ActorGenerator.__subclasses__()

        for actor_generator_class in actor_generators_list:
            self._registered_generators.append(actor_generator_class())

        # raises exception if no generator was found
        if len( self._registered_generators ) < 1:
            raise RuntimeError( 'ERROR! No valid generator plug-in can be found!' )

