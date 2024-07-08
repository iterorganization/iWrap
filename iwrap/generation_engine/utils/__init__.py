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

    for generator_class in generators_class_list:

        generator_name = "<unknown>"
        try:
            generator_name = generator_class.__module__ + generator_class.__qualname__
            generator_class.check_api_compliance()

            generator_object = generator_class()
            generator_name = generator_object.name
            generators_list.append(generator_object)

        except RuntimeWarning as exc:
            __logger.warning(exc )
            continue

        except Exception as exc:
            msg = f'Error while loading "{generator_name}" (type: {generator_base_class.__name__}) plug-in. '
            __logger.exception( msg, exc_info=exc )
            continue

    return generators_list
