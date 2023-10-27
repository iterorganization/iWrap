import yaml
import argparse
from .mappings import mappings
from os import rename
from os.path import splitext
import logging
import re
from iwrap.common.utils import resolve_variable


class Converter:
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self, yaml_dict):
        self._yaml_dict = yaml_dict

    def value_of(self, path):
        return self.__get(path.split("/"), None)

    def type_of(self, path):
        return type(self.__get(path.split("/"), None))

    def sys_var(self, name):
        #used to get system variables in simple way

        if not name.startswith('$'):
            name = '$'+name

        return resolve_variable(name)

    def __delete(self, splitted_path, condition, current_node=None):
        """
          Deletes key from dict.
          Args:

           current_node (dict): Current_node in the dictionary.
           splitted_path (list): List of the following nodes (path to node).
           filter (str): Code used as additional filter for key to be deleted.

           Returns:
               True - if something was deleted
               False - if nothing was deleted (eg. node to delete does not exists)
        """

        '''
        Function's workflow is splitted into 2 paths:
        - if current_node is last but one on the splitted_path list - filter and delete key
        - else - jump along path to next node (recursive call)
        '''

        if current_node is None:
            current_node = self._yaml_dict

        if len(splitted_path) == 1:
            if condition is None:
                try:
                    del current_node[splitted_path[0]]
                except (TypeError,KeyError):
                    #Cannot be deleted because does not exists, proceed
                    return False
                return True
            else:
                if isinstance(current_node[splitted_path[0]], list):
                    condition = condition.replace("$TARGET", "current_node[splitted_path[0]][x]")
                    del_indexes = []

                    for x in range(len(current_node[splitted_path[0]])):
                        if eval( condition ):
                            del_indexes.append(x)

                    # Sort the indexes in descending order to prevent index shifting
                    del_indexes.sort(reverse=True)

                    for i in del_indexes:
                        del current_node[splitted_path[0]][i]
                    else:
                        #if nothing was deleted return false
                        if current_node[splitted_path[0]] == []:
                            del current_node[splitted_path[0]]
                        return False

                    if current_node[splitted_path[0]] == []:
                        del current_node[splitted_path[0]]
                    return True
                else:
                    condition = condition.replace("$TARGET", "current_node[splitted_path[0]]")
                    if eval( condition ):
                        try:
                            del current_node[splitted_path[0]]
                        except (TypeError, KeyError):
                            # Cannot be deleted because does not exists, proceed
                            return False
                    return True
        else:
            try:
                new_current_node = current_node.get(splitted_path[0])
            except  KeyError:
                return False
                # raise KeyError(f'node \"{splitted_path[0]}\" not found') from None

            if isinstance(new_current_node, list):
                for x in new_current_node:
                    self.__delete(splitted_path[1:], condition, current_node=x)
            else:
                self.__delete(splitted_path[1:], condition, current_node=new_current_node)

    def __get(self, splitted_path, condition, current_node=None):
        """
          Gets node from dict.
          Args:
           current_node (dict): Current node being processed.
           splitted_path (list): List of the following nodes (path to node).
           condition (str): Code used as additional filter for node to be extracted.
        """

        if current_node is None:
            current_node = self._yaml_dict

        # if we are at the end of path_to_node
        if len(splitted_path) == 1:

            if condition is None:
                # if there is no filter, just return what you got
                try:
                    res = current_node[splitted_path[0]]
                except (TypeError, KeyError):
                    # if there is no node from path
                    return None
                return res
            else:
                # save result
                res = current_node[splitted_path[0]]
                #if result contains more than one entry, run evaluate string on it to throw unnecesary ones
                if isinstance(res,list):
                    condition = condition.replace("$SOURCE", "x")
                    res2 = []
                    for x in res:
                        if(eval(condition)):
                            res2.append(x)
                    return res2

                elif isinstance(res,(dict, str, int, float)):
                    condition = condition.replace("$SOURCE", "res")
                    if eval(condition):
                        return res
                    return None
                elif res is None:
                    return res
                else:
                    raise Exception(f'Not supported node type: {type(res)}')

        #if there is something more in path_to_node
        try:
            new_current_node = current_node[splitted_path[0]]
        except  KeyError:
            return None
            #raise KeyError(f'node \"{splitted_path[0]}\" not found') from None
        return self.__get(splitted_path[1:], condition, current_node=new_current_node)

    def __set(self, splitted_path, value, condition, current_node=None, create_node=True):
        """
          Sets value in dict.
          Args:
           splitted_path (list): List of the following nodes (path to node).
           value: Value to be set. Could be literal, list or dict.
           condition (str): Code used as condition to be fullfilled in order to execute command.
           create_node: Flag to create new node if does not exists.
        """

        #__set does not uses $SOURCE and $TARGET variables, so condition can be evaluated at the beginning
        if condition is not None and not eval(condition):
            return False

        if current_node is None:
            current_node=self._yaml_dict

        if len(splitted_path) == 1:

            #evaluate value
            if isinstance(value, str) and '$SYS_VAR' in value:
                value = value.replace("$SYS_VAR", "self.sys_var")
                value = eval(value)

            if splitted_path[0] in current_node:
                current_node[splitted_path[0]] = value
                return True
            else:
                if create_node:
                    current_node[splitted_path[0]] = value
                    return True
                else:
                    return False

        else:
            try:
                if current_node[splitted_path[0]] is None:
                    if create_node:
                        current_node[splitted_path[0]] = dict()
                    else:
                        return False
            except  KeyError:
                current_node.setdefault(splitted_path[0], dict())

            if isinstance(current_node[splitted_path[0]], str):
                current_node[splitted_path[0]] = dict()
            return self.__set(splitted_path[1:], value, condition, current_node=current_node[splitted_path[0]])


    @staticmethod
    def convert(yaml_dict, command_line=False):
        """
          Main method used to convert yaml using mappings.
          NOTE: Mappings are read from mappings package.

          Args:
           yaml_dict (dict): Dictionary to be converted.
           command_line (bool): Indicates if function was called manually by user (true), or called implicit by iWrap (false).
        """

        converter = Converter(yaml_dict)

        transform_counter = 0
        for mapping in mappings:
            logging.info(f'Processing mapping: {mapping}')
            command = mapping["command"].lower()

            try:
                condition = mapping["condition"]
                condition = condition.replace("$VALUE_OF", "self.value_of")
                condition = condition.replace("$TYPE_OF", "self.type_of")
                condition = condition.replace("$SYS_VAR", "self.sys_var")
            except KeyError:
                condition = None

            if command == 'add':
                target = mapping["target"]
                try:
                    value = mapping["value"]
                except KeyError:
                    value = None

                if converter.__set(target.split('/'), value, condition):
                    transform_counter+=1

            elif command == 'set':
                target = mapping["target"]
                value = mapping["value"]

                if converter.__set(target.split('/'), value, condition, create_node=False):
                    transform_counter+=1

            elif command == 'delete':
                target = mapping["target"]
                try:
                    if converter.__delete(target.split('/'), condition):
                        transform_counter += 1
                except KeyError:
                    continue

            elif command == 'move':
                source = mapping["source"]
                target = mapping["target"]

                try:
                    source_value = converter.__get(source.split('/'), condition)
                except KeyError:
                    continue

                if source_value is None:
                    continue

                #__delete function uses $TARGET keyword instead of $SOURCE, so it must be changed
                delete_condition = None
                if condition is not None:
                    delete_condition = condition.replace('$SOURCE','$TARGET')

                if converter.__delete(source.split('/'), delete_condition):
                    transform_counter += 1

                if converter.__set(target.split('/'), source_value, None):
                    transform_counter += 1


            else:
                message = f'MAPPING: {mapping} COULD NOT BE PROCESSED. COMMAND {command} UNRECOGNISED.'
                raise RuntimeError(message)

        if not command_line and transform_counter>0:
            converter.__logger.warning('[WARNING]: You are using outdated code description.'
                                       ' Consider using iwrap-yaml-update script to keep your description up to date.')
        return yaml_dict


class NoAliasDumper(yaml.SafeDumper):
    def ignore_aliases(self, data):
        return True

    #dump empty sequence as: ' ' (nothing)
    def represent_sequence(self, tag, sequence, flow_style=None):
        if not sequence:  # Check if the sequence is empty
            return self.represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_sequence(tag, sequence, flow_style)

    # dump empty dict as: ' ' (nothing)
    def represent_mapping(self, tag, mapping, flow_style=None):
        if not mapping:  # Check if the sequence is empty
            return self.represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_mapping(tag, mapping, flow_style)

#make None values being saves as: ' ' (nothing)
NoAliasDumper.add_representer(
    type(None),
    lambda dumper, value: dumper.represent_scalar(u'tag:yaml.org,2002:null', '')
  )

def main():
    class IllegalArgumentError(ValueError):
        pass

    # ------- PROCESS INPUT ARGUMENTS -------
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--file", help="Input filename", type=str, required=True)
    parser.add_argument("-o", "--output", help="Output filename", type=str)
    parser.add_argument("--verbose", help="Verbose", action="store_const", dest="loglevel", const=logging.INFO,)

    parser.add_argument("-i", "--in-place", help="Input is renamed as <input>_old. Output is saved under input\'s filename.", action='store_true')
    args = parser.parse_args()

    logging.basicConfig(level=args.loglevel)

    if args.output and args.in_place:
        message = '\n-o/--output and -i/--in-place arguments cannot be used together. Output can be saved under input\'s filename or under new one, not both.'
        raise IllegalArgumentError(message)

    input_filename = args.file

    # ------- LOAD INPUT DATA -------
    with open(input_filename, 'r') as file:
        try:
            data = file.read()
        except FileNotFoundError as e:
            logging.error(e)
            exit(0)

    yaml_dict = yaml.load(data, Loader=yaml.Loader)

    result_dict = Converter.convert(yaml_dict, command_line=True)
    logging.info(f'Result dict:\n{result_dict}')

    if args.in_place:
        output_filename = input_filename
        rename(input_filename,splitext(input_filename)[0] + '_old' + splitext(input_filename)[1])
    elif args.output:
        output_filename = args.output
    else:
        print(result_dict)
        exit(0)

    # ------- SAVE OUTPUT -------
    file = open(output_filename, "w")
    yaml.dump(yaml_dict, file, Dumper=NoAliasDumper)
    file.close()
    exit(0)

if __name__ == '__main__':
    main()
