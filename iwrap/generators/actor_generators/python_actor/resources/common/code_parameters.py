import logging
from iwrap.settings.code_parameters_handlers.handler_factory import HandlerFactory


class CodeParameters:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    @property
    def schema(self):
        return self.__handler.schema

    @property
    def parameters(self):
        return self.__handler.parameters

    @property
    def parameters_path(self):
        return self.__handler.parameters_path

    @parameters_path.setter
    def parameters_path(self, path: str) -> None:
        self.__handler.parameters_path = path

    @property
    def format(self):
        return self._parameters_format

    def initialize(self):
        self.__handler.initialize(self._default_parameters_path, self._schema_path)

    def get_parameter(self, path_to_node: str) -> str:
        return self.__handler.get_parameter(path_to_node)

    def set_parameter(self, path_to_node: str, value) -> None:
        self.__handler.set_parameter(path_to_node, value)

    def __init__(
        self, default_parameters_path: str, schema_path: str, parameters_format="xml"
    ):
        from pathlib import Path as PathLib

        # Resolve paths relative to actor's input directory
        # Actor structure: actor_name/common/code_parameters.py and actor_name/input/
        # Use resolve() to get the absolute real path, handling symlinks
        actor_dir = (
            PathLib(__file__).resolve().parent.parent
        )  # Go up from common/ to actor root
        input_dir = actor_dir / "input"

        self.__logger.debug(f"Actor directory: {actor_dir}")
        self.__logger.debug(f"Input directory: {input_dir}")
        self.__logger.debug(f"Input params path: {default_parameters_path}")
        self.__logger.debug(f"Input schema path: {schema_path}")

        # Convert relative paths (filenames) to absolute paths in actor's input directory
        if default_parameters_path:
            params_path = PathLib(default_parameters_path)
            if not params_path.is_absolute():
                default_parameters_path = str(input_dir / params_path)
                self.__logger.debug(f"Resolved params path: {default_parameters_path}")

        if schema_path:
            schema_path_obj = PathLib(schema_path)
            if not schema_path_obj.is_absolute():
                schema_path = str(input_dir / schema_path_obj)
                self.__logger.debug(f"Resolved schema path: {schema_path}")

        self._default_parameters_path = default_parameters_path
        self._schema_path = schema_path

        self._parameters_format = parameters_format.lower()
        self.__handler = HandlerFactory.get_handler(parameters_format)
        self.__handler.initialize(default_parameters_path, schema_path)

    def restore_default_parameters_path(self):
        self.__handler.restore_default_parameters_path()

    def validate(self):
        self.__handler.validate()
