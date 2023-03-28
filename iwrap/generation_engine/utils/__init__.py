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

    except ModuleNotFoundError as exc:
        __logger.exception(f'No built-in {generator_base_class.__name__} plug-ins have been loaded:\n{exc}' )
    except Exception as exc:
        __logger.exception(f'Error while loading built-in {generator_base_class.__name__} plug-in {name}"')
        raise

    try:
        generator_module = importlib.import_module( plugin_pkg_name )
        packages = pkgutil.walk_packages( generator_module.__path__, generator_module.__name__ + "." )
        for finder, name, ispkg in packages:
            try:
                importlib.import_module( name )
                __logger.info(f'External {generator_base_class.__name__} plug-in found: "{name}"')
            except:
                __logger.exception(f'Error while loading external {generator_base_class.__name__} plug-in "{name}"')

    except ModuleNotFoundError as exc:
        __logger.info(f'No external {generator_base_class.__name__} plug-ins have been found' )

    generators_class_list = generator_base_class.__subclasses__()

    for actor_generator_class in generators_class_list:
        generators_list.append( actor_generator_class() )

    return generators_list
