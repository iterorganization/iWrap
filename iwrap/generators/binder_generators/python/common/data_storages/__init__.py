import importlib
import pkgutil
from typing import List, Set

from iwrap.generation_engine import utils


class IDSConverter():

    @classmethod
    def data_type(self) -> str:
        ...

    @classmethod
    def code_languages(self) -> Set[str]:
        ...

    def convert_to_native_type(self):
        ...

    def convert_to_actor_type(self):
        ...

    def save(self, stream):
        ...

    def release(self):
        ...


def discover_converters(builtin_pkg_name: str, plugin_pkg_name: str, converter_base_class):
    converters_list = []
    try:
        converter_module = importlib.import_module( builtin_pkg_name )
        packages = pkgutil.walk_packages( converter_module.__path__, converter_module.__name__ + "." )
        for finder, name, ispkg in packages:
            importlib.import_module( name )

    except Exception as exc:
        print( f'INFO: No built-in {converter_base_class.__name__} plug-ins have been loaded:\n{exc}' )

    try:
        converter_module = importlib.import_module( plugin_pkg_name )
        packages = pkgutil.walk_packages( converter_module.__path__, converter_module.__name__ + "." )
        for finder, name, ispkg in packages:
            importlib.import_module( name )

    except Exception as exc:
        print( f'INFO: No external {converter_base_class.__name__} plug-ins have been found' )

    converters_class_list = converter_base_class.__subclasses__()

    return converters_class_list

class IDSConvertersRegistry:
    __builtin_pkg_name: str = '.'.join(__name__.split('.')[:-1])
    __plugin_pkg_name: str = 'iwrap_ids_converter'
    __converters: List[IDSConverter] = []

    @classmethod
    def initialize(cls):
        cls.__converters = discover_converters( cls.__builtin_pkg_name, cls.__plugin_pkg_name, IDSConverter )

        # raises exception if no converter was found
        if len( cls.__converters ) < 1:
            raise RuntimeError( 'ERROR! No valid ids converter can be found!' )

    @classmethod
    def get_converter_class(cls, data_type: str, code_language: str) -> IDSConverter:

        for converter_class in cls.__converters:
            if data_type == converter_class.data_type() and code_language in converter_class.code_languages():
                return converter_class

        types = [(converter_class.data_type(), converter_class.code_language()) for converter in cls.__converters]
        raise ValueError( f'ERROR: No converter found to bind ids of type "{data_type}" to "{code_language}" '
                          f'! Registered converters: "{types}".' )