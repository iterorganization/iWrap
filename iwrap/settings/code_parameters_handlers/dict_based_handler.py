from .generic_handler import GenericHandler
from abc import ABC, abstractmethod
import collections.abc
from typing import Set


'''
    DictBasedHandler is a template class for all dict-based code parameres formats like json, yaml, fortran namelist
'''
class DictBasedHandler(GenericHandler, ABC):

    def __init__(self):
        super(DictBasedHandler, self).__init__()

    def _get_dict_value(self, splitted_path, parameters_dict):
        if len(splitted_path) == 0:
            return dict

        current_path_without_index, index, rest_of_splitted_path = self._extract_path_info(splitted_path)

        try:
            if index is not None:
                if not isinstance(dict[current_path_without_index], collections.abc.Sequence):
                    raise IndexError(f"Parameters node {dict[current_path_without_index]} is not an array and cannot be accessed using array index.")
                return self._get_dict_value(rest_of_splitted_path, parameters_dict[current_path_without_index][index])
            else:
                return self._get_dict_value(rest_of_splitted_path,parameters_dict[current_path_without_index])
        except KeyError:
            self.__logger.error(f"KeyError. Cannot find {current_path_without_index[0]} key in parameters document.")

    def _set_dict_value(self, splitted_path, parameters_dict, value):
        current_path_without_index, index, rest_of_splitted_path = self._extract_path_info(splitted_path)

        #if we are one dictionary node before target
        if len(rest_of_splitted_path) == 0:
            if index is not None:
                if not isinstance(parameters_dict[current_path_without_index], collections.abc.Sequence):
                    raise IndexError(f"Parameters node {parameters_dict[current_path_without_index]} is not an array and cannot be accessed using array index.")
                try:
                    parameters_dict[current_path_without_index][index] = value
                except IndexError:
                    parameters_dict[current_path_without_index].append(value)
            else:
                parameters_dict[current_path_without_index] = value
            return
        #if there are more nodes in path just jump along path
        else:
            try:
                if index is not None:
                    self._set_dict_value(rest_of_splitted_path, parameters_dict[current_path_without_index][index], value)
                else:
                    self._set_dict_value(rest_of_splitted_path, parameters_dict[current_path_without_index], value)
            except KeyError:
                self.__logger.error(f"KeyError. Cannot find {splitted_path[0]} key in parameters document.")

    def get_parameter(self, path_to_node: str) -> str:
        if not self._default_parameters_path:
            return

        if not self._parameters_str  or self._new_path_set:
            self.initialize(self._parameters_path, self._schema_path)

        splitted_path = path_to_node.split("/")
        parameters_dict = self.to_dict()
        return self._get_dict_value(splitted_path, parameters_dict)

    def set_parameter(self, path_to_node: str, value) -> None:
        if not self._default_parameters_path:
            return

        if not self._parameters_str  or self._new_path_set:
            self.initialize(self._parameters_path, self._schema_path)

        splitted_path = path_to_node.split("/")
        parameters_dict = self.to_dict()
        self._set_dict_value(splitted_path, parameters_dict, value)
        self.from_dict(parameters_dict)



    @abstractmethod
    def to_dict(self):
        '''
                Returns self.parameters converted to python dictionary
        '''
        ...

    @abstractmethod
    def from_dict(self, dict):
        '''
                Converts dictionary passed as argument to raw string parameters (self.parameters)
        '''
        ...

    @abstractmethod
    def validate(self):
        ...

    @property
    @abstractmethod
    def formats(self) -> Set[str]:
        ...