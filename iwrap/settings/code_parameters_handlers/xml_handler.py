from .generic_handler import GenericHandler
from xml.etree import ElementTree as et
from lxml import etree
from pathlib import Path
from typing import Set

class XMLHandler(GenericHandler):

    @property
    def formats(self) -> Set[str]:
        return {'xml', 'legacy-xml'}

    def __is_leaf(self, node):
        return len(list(node)) < 1

    def __get_xml_value(self, splitted_path, tree):
        current_path_without_index, index, rest_of_splitted_path = self._extract_path_info(splitted_path)
        # if index == None => index = 0
        index = index or 0

        # if there is only one element in splitted path
        if len(splitted_path) == 1:
            if current_path_without_index == tree.tag:
                if self.__is_leaf(tree):
                    return tree.text.strip()
                else:
                    raise Exception(f'Node <{tree.tag}> is not a leaf node, thus it cannot be last element in provided path')

        # if there are more elements in splitted path AND current node is a leaf
        elif self.__is_leaf(tree):
            raise Exception(f'Node <{tree.tag}> is already XML leaf, but remaining path \"{rest_of_splitted_path}\" contains child nodes')

        # if there are more elements in splitted path AND current node is NOT a leaf
        else:
            found_nodes = tree.findall(rest_of_splitted_path[0])
            if index+1 > len(found_nodes):
                raise Exception(f'XML node was not found. Searched node <{current_path_without_index}>, found {len(found_nodes)} nodes, tried to access index [{index}]')

            return self.__get_xml_value(rest_of_splitted_path, found_nodes[index])

    def __set_xml_value(self, splitted_path, tree, value):

        if len(splitted_path) == 1:
            if splitted_path[0] == tree.tag:
                if self.__is_leaf(tree):
                    tree.text = str(value)
                    return
                else:
                    raise Exception(f'Node <{tree.tag}> is not a leaf node, thus it\'s value cannot be set.)')

        current_path_without_index, index, rest_of_splitted_path = self._extract_path_info(splitted_path)
        # if index == None => index = 0
        index = index or 0

        current_node_children_arr = tree.findall(rest_of_splitted_path[0])

        # add new node(s) if needed
        if len(current_node_children_arr) <= index:
            number_of_nodes_to_add = (index+1) - len(current_node_children_arr)

            for _ in range(number_of_nodes_to_add):
                new_node = et.Element(current_path_without_index)
                tree.append(new_node)

            if len(splitted_path) == 1:
                #set value of last added node
                new_node.text = str(value)
                return

        return self.__set_xml_value(rest_of_splitted_path, tree.findall(rest_of_splitted_path[0])[index], value)

    def get_parameter(self, path_to_node:str) -> str:

        if not self._default_parameters_path:
            return

        if not self._parameters_str  or self._new_path_set:
            self.initialize(self._default_parameters_path, self._schema_path)

        tree = et.ElementTree(et.fromstring(self._parameters_str))
        root = tree.getroot()

        if path_to_node[0] == '/':
            path_to_node = path_to_node[1:]
        path_root = path_to_node.split('/')[0]

        if path_root != root.tag:
            raise Exception(f'Path root tag does not match XML document root. Path root is "{path_root}", while document root is "{root.tag}"')

        splitted_path = path_to_node.split('/')

        return self.__get_xml_value(splitted_path, root)

    def set_parameter(self, path_to_node:str, value) -> None:
        if not self._default_parameters_path:
            return
        if not self._parameters_str or self._new_path_set:
            self.initialize(self._default_parameters_path, self._schema_path)

        #replace arrays with XML lists e.g. [1,2,3] -> "1 2 3"
        cleared_value = value
        if isinstance(value, list):
            cleared_value = ' '.join(map(str, value))

        # add  operations on XML tree
        tree = et.ElementTree(et.fromstring(self._parameters_str))
        root = tree.getroot()

        splitted_path = path_to_node.split('/')

        self.__set_xml_value(splitted_path, root, cleared_value)
        self._parameters_str = et.tostring(tree.getroot()).decode('utf-8')


    def __init__(self):
        super().__init__()

    def validate(self):
        if not self._parameters_str or self._new_path_set:
            self.initialize(self._parameters_path, self._schema_path)

        xml_schema_tree = etree.fromstring(bytes(self._schema_str, encoding='utf8'))
        xml_schema_validator = etree.XMLSchema(xml_schema_tree)

        # Parse XML file:
        xml_tree = etree.fromstring(bytes(self._parameters_str, encoding='utf8'))

        try:
            # Perform validation:
            xml_schema_validator.assertValid(xml_tree)
        except etree.DocumentInvalid:
            message = "\n\nXML validation error(s):\n"

            if self.parameters_path:
                message = message + f'File: {self.parameters_path}\n'
            for error in xml_schema_validator.error_log:
                message = message + f'  Line {error.line}: {error.message} \n'
            raise etree.DocumentInvalid(message) from None
