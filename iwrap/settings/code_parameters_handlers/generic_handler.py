from .parameters_handler_interface import ParametersHandlerInterface
import logging
from pathlib import Path
from abc import ABC, abstractmethod
import re
from typing import Set

class GenericHandler(ParametersHandlerInterface, ABC):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):
        self._default_params_dir = Path(Path(__file__).parent, '../../input/')
        self._new_path_set: bool = True
        self._parameters_path: str = None
        self._default_parameters_path: str = None

        self._schema_str = None
        self._parameters_str = None

    def _read_file(self, file_path):
        with open( file_path, mode='rt', encoding='utf-8' ) as file:
            file_str = file.read()

        return file_str

    def _extract_path_info(self, splitted_path):
        '''
        Args:
            Splitted path [str] - list of strings defining path to target node. Every string may contain array access operator ('()') e.g. ['path(0)','to(1)','node(2)']

        Returns:
            Tuple
            - First element of splitted path with array access operator subtracted
            - Extracted index from first element's array access operator
            - The rest of the splited path, with first element subtracted
        '''
        #pattern of code_parameters array access operator
        pattern = r'\(-?\d+\)'
        # first element of path with removed index (if existed)
        current_path_without_index = re.sub(pattern=pattern, repl='', string=splitted_path[0])

        index = None
        #search for index to extract it
        if re.search(pattern, splitted_path[0]):
            array_index_string = re.search(pattern, splitted_path[0]).group()
            index = int(re.search(r'-?\d+', array_index_string).group())

            if index < 0:
                raise IndexError('XML path index cannot be negative')

        rest_of_splitted_path = splitted_path[1:]

        return (current_path_without_index, index, rest_of_splitted_path)

    @property
    def schema(self):
        if not self._schema_str:
            raise AttributeError("Parameters handler not initialized or empty schema file provided. Cannot access `schema` attribute.")
        return self._schema_str

    @property
    def parameters(self):
        if not self._parameters_str:
            raise AttributeError("Parameters handler not initialized or empty parameters file provided. Cannot access `parameters` attribute.")
        if self._new_path_set:
            self.initialize(self._parameters_path, self._schema_path)
        return self._parameters_str

    @property
    def parameters_path(self):
        return self._parameters_path

    @parameters_path.setter
    def parameters_path(self, path: str) -> None:
        if not Path(path).is_absolute():
            path = Path(path).resolve()

        self._parameters_path = path
        self._new_path_set = True

        self.initialize(path, self._schema_path)

    @property
    @abstractmethod
    def formats(self) -> Set[str]:
        ...

    @abstractmethod
    def get_parameter(self, path_to_node: str) -> str:
        ...

    @abstractmethod
    def set_parameter(self, path_to_node: str, value) -> None:
        ...

    @abstractmethod
    def validate(self):
        ...

    def initialize(self, parameters_path: str, schema_path: str):
        if not self._default_parameters_path:
            self._default_parameters_path = parameters_path

        self._parameters_path = parameters_path
        self._schema_path = schema_path

        # Read XSD (if not yet loaded)
        if not self._schema_str:
            schema_path = Path( self._default_params_dir, Path(self._schema_path))
            self._schema_str = self._read_file( schema_path )

        if self._new_path_set:
            if self._parameters_path:
                parameters_path = Path( self._default_params_dir, Path(self._parameters_path))
                self._parameters_str = self._read_file(parameters_path)
                self._new_path_set = False

    def restore_default_parameters_path(self):
        self._parameters_path = self._default_parameters_path
        self._new_path_set = True

        self.initialize(self._default_parameters_path, self._schema_path)
