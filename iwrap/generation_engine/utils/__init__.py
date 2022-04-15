import importlib
import pkgutil
import logging

# Class logger
import traceback

__logger = logging.getLogger( __name__ )

def discover_generators(builtin_pkg_name: str, plugin_pkg_name: str, generator_base_class):
    generators_list = []
    try:
        generator_module = importlib.import_module( builtin_pkg_name )
        packages = pkgutil.walk_packages( generator_module.__path__, generator_module.__name__ + "." )
        for finder, name, ispkg in packages:
            importlib.import_module( name )

    except Exception as exc:
        print(traceback.format_exc())
        __logger.warning(f'No built-in {generator_base_class.__name__} plug-ins have been loaded:\n{exc}' )

    try:
        generator_module = importlib.import_module( plugin_pkg_name )
        packages = pkgutil.walk_packages( generator_module.__path__, generator_module.__name__ + "." )
        for finder, name, ispkg in packages:
            importlib.import_module( name )

    except Exception as exc:
        __logger.warning(f'INFO: No external {generator_base_class.__name__} plug-ins have been found' )

    generators_class_list = generator_base_class.__subclasses__()

    for actor_generator_class in generators_class_list:
        generators_list.append( actor_generator_class() )

    return generators_list
