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
        if not self.__schema_str:
            self.initialize()
        return self.__schema_str

    @property
    def parameters(self):
        if not self.__parameters_str  or self.__new_path_set:
            self.initialize()
        return self.__parameters_str

    @property
    def parameters_path(self):
        return self.__parameters_path

    @parameters_path.setter
    def parameters_path(self, path: str) -> None:
        if not Path(path).is_absolute():
            path = Path(path).resolve()

        self.__parameters_path = path
        self.__new_path_set = True

    def __is_leaf(self, node):
        return len(list(node)) < 1

    def __get_tree_info(self, path):
        pattern = r'\((\d)\)'

        # get current node and index
        split_path = path.split('/')
        current_node = split_path[1]
        index = 0

        # if index in current node
        if re.search(pattern, current_node):
            array_index_string = re.search(r'\(\d+\)', current_node).group()
            index = int(re.search(r'\d+', array_index_string).group())
            current_node = current_node[:-3]

        # remove current node from path and run function again
        path = "/".join(split_path[1:])

        return path, index, current_node

    def __get_xml_value(self, path, tree):
        if self.__is_leaf(tree):
            #remove index from path to compare node.tag with path
            pattern = r'\([0-9]+\)'
            cutted_path = re.sub(pattern, '', path)

            if cutted_path.replace('/','') == tree.tag:
                return tree.text.strip()
            else:
                print(f'tag: {tree.tag} : {tree.text.strip()}')
                raise Exception(f'Node <{tree.tag}> is already XML leaf, but remaining path \"{path}\" contains child nodes')


        path, index, current_node = self.__get_tree_info(path)

        if index < 0:
            raise IndexError('XML path index cannot be negative')

        found_nodes = tree.findall(current_node)
        if index+1 > len(found_nodes):
            raise Exception(f'XML node was not found. Searched node <{path}>, found {len(found_nodes)} nodes, tried to access index [{index}]')

        return self.__get_xml_value(path, found_nodes[index])

    def __set_xml_value(self, path, tree, value):
        cleared_value = value
        if isinstance(value, list):
            cleared_value = ' '.join(map(str, value))

        pattern = r'\([0-9]+\)'
        cutted_path = re.sub(pattern, '', path)

        splitted_path = cutted_path.split('/')

        if tree.tag == splitted_path[-1]:
            tree.text = str(cleared_value)
            return

        path, index, current_node = self.__get_tree_info(path)

        current_node_children = tree.findall(current_node)

        if len(current_node_children) <= index:
            number_of_nodes_to_add = (index+1) - len(current_node_children)

            for x in range(number_of_nodes_to_add):
                new_node = et.Element(current_node)
                tree.append(new_node)


        return self.__set_xml_value(path, tree.findall(current_node)[index], cleared_value)

    def get_parameter(self, path_to_node:str) -> str:
        if not self.__default_parameters_path:
            return

        if not self.__parameters_str  or self.__new_path_set:
            self.initialize()

        tree = et.ElementTree(et.fromstring(self.__parameters_str))
        root = tree.getroot()

        if path_to_node[0] == '/':
            path_to_node = path_to_node[1:]
        path_root = path_to_node.split('/')[0]

        if path_root != root.tag:
            raise Exception(f'Path root tag does not match XML document root. Path root is "{path_root}", while document root is "{root.tag}"')

        return self.__get_xml_value(path_to_node, root)

    def set_parameter(self, path_to_node:str, value) -> None:
        if not self.__default_parameters_path:
            return
        if not self.__parameters_str or self.__new_path_set:
            self.initialize()

        # add  operations on XML tree
        tree = et.ElementTree(et.fromstring(self.__parameters_str))
        root = tree.getroot()

        self.__set_xml_value(path_to_node, root, value)
        self.__parameters_str = et.tostring(tree.getroot()).decode('utf-8')


    def __init__(self, default_parameters_path:str, schema_path:str):

        self.__default_params_dir = Path( Path( __file__ ).parent, '../input/')
        self.__default_parameters_path = default_parameters_path
        self.__schema_path = schema_path

        self.__new_path_set:bool = True
        self.__parameters_path: str = None

        self.__schema_str = None
        self.__parameters_str = None

    def _read_file(self, file_path):
        with open( file_path, mode='rt', encoding='utf-8' ) as file:
            file_str = file.read()

        return file_str

    def initialize(self):
        # actor with parameters MUST have default parameters
        if not self.__default_parameters_path:
            return

        # Read XSD (if not yet loaded)
        if not self.__schema_str:
            schema_path = Path( self.__default_params_dir, Path(self.__schema_path))
            self.__schema_str = self._read_file( schema_path )


        if self.__new_path_set:
            if self.parameters_path:
                self.__parameters_str = self._read_file( self.parameters_path )
            else:
                default_parameters_path = Path( self.__default_params_dir, Path( self.__default_parameters_path ) )
                self.__parameters_str = self._read_file( default_parameters_path )

            self.__new_path_set = False

        self.validate()

    def validate(self):
        xml_schema_tree = etree.fromstring(bytes(self.__schema_str, encoding='utf8'))
        xml_schema_validator = etree.XMLSchema( xml_schema_tree )

        # Parse XML file:
        xml_tree = etree.fromstring(bytes(self.__parameters_str, encoding='utf8'))

        # Perform validation:
        xml_schema_validator.assertValid( xml_tree )
