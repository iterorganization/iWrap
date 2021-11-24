import logging
from pathlib import Path
from xml.etree import ElementTree as et
import re
from lxml import etree


class CodeParameters:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    @property
    def schema(self):
        return self.__schema_str

    @property
    def parameters(self):
        return self.__parameters_str

    def is_leaf(self, node):
        return len(list(node)) < 1

    def get_tree_info(self, path):
        pattern = r'\((\d)\)'

        # get current node and index
        split_path = path.split('/')
        current_node = split_path[1]
        index = 0

        # if index in current node
        if re.search(pattern, current_node):
            index = int(re.search(r'\d+', current_node).group()) - 1
            current_node = current_node[:-3]

        # remove current node from path and run function again
        path = "/".join(split_path[1:])

        return path, index, current_node

    def get_xml_value(self, path, tree):
        if self.is_leaf(tree):
            return tree.text

        path, index, current_node = self.get_tree_info(path)
        self.get_xml_value(path, tree.findall(current_node)[index])

    def set_xml_value(self, path, tree, value):
        if self.is_leaf(tree):
            tree.text = value
            return

        path, index, current_node = self.get_tree_info(path)
        self.set_xml_value(path, tree.findall(current_node)[index], value)

    def get_parametr_value(self, path_to_node:str) -> str:
        if not self.__default_parameters_path:
            return

        if not self.__parameters_str:
            self.initialize()

        tree = et.ElementTree(et.fromstring(self.__parameters_str))
        root = tree.getroot()
        return self.get_xml_value(path_to_node, root)

    def set_parametr_value(self, path_to_node:str, value) -> None:
        if not self.__default_parameters_path:
            return
        if not self.__parameters_str:
            self.initialize()

        # add  operations on XML tree
        tree = et.ElementTree(et.fromstring(self.__parameters_str))
        root = tree.getroot()
        self.set_xml_value(path_to_node, root, value)

    def __init__(self, default_parameters_path:str, schema_path:str):

        self.__default_params_dir = Path( Path( __file__ ).parent, '../input/')
        self.__default_parameters_path = default_parameters_path
        self.__schema_path = schema_path

        self.parameters_path = None

        self.__schema_str = None
        self.__parameters_str = None

    def _read_file(self, file_path):
        with open( file_path, mode='rt', encoding='utf-8' ) as file:
            file_str = file.read()

        return file_str

    def initialize(self):
        if not self.__default_parameters_path:
            return

        schema_path = Path( self.__default_params_dir, Path(self.__schema_path))
        self.__schema_str = self._read_file( schema_path )

        if self.parameters_path:
            self.__parameters_str = self._read_file( self.parameters_path )
        else:
            default_parameters_path = Path( self.__default_params_dir, Path( self.__default_parameters_path ) )
            self.__parameters_str = self._read_file( default_parameters_path )

        self.validate()

    def validate(self):
        xml_schema_tree = etree.fromstring( self.__schema_str )
        xml_schema_validator = etree.XMLSchema( xml_schema_tree )

        # Parse XML file:
        xml_tree = etree.fromstring( self.__parameters_str )

        # Perform validation:
        xml_schema_validator.assertValid( xml_tree )




