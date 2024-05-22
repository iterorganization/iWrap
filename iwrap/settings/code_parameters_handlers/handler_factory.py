from .xml_handler import XMLHandler
from .json_handler import JsonHandler
from .fortran_namelist_handler import FortranNamelistHandler
from .parameters_handler_interface import ParametersHandlerInterface

from typing import Set

class HandlerFactory:
    _handlers = {XMLHandler, JsonHandler, FortranNamelistHandler}

    def __init__(self):
        ...

    @staticmethod
    def _get_handler_class(handler_name):
        for handler in HandlerFactory._handlers:
            handler_instance = handler()
            if handler_name.lower() in handler_instance.formats:
                return handler

        raise ValueError(
            f'{handler_name} is not valid format of code parameters. Currently loaded parameters handlers formats are: {HandlerFactory.get_formats()}')

    @staticmethod
    def get_handler(handler_name) -> ParametersHandlerInterface:
        handler_class = HandlerFactory._get_handler_class(handler_name or 'xml')
        handler_instance = handler_class()
        return handler_instance

    @staticmethod
    def get_formats() -> Set[str]:
        formats = set()
        for handler in HandlerFactory._handlers:
            handler_instance = handler()
            formats.update(handler_instance.formats)

        return formats
