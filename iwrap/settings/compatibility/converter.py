import yaml
import argparse
from .mappings import mappings
from os import rename
from os.path import splitext

class Converter:
    @staticmethod
    def __delete(root, splitted_path, filter):
        '''
          Deletes key from dict.
          Args:
           root (dict): Root of the dictionary.
           splitted_path (list): List of the following nodes (path to node).
           filter (str): Code used as additional condition for key to be deleted.
        '''

        '''
        Function's workflow is splitted into 2 paths:
        - if root is last but one on the splitted_path list - filter and delete key
        - else - just jump along path to next node (recursive call)
        '''

        if len(splitted_path) == 1:
            if filter == None:
                del root[splitted_path[0]]
                return
            else:
                if isinstance(root[splitted_path[0]], list):
                    filter = filter.replace("arg1", "root[splitted_path[0]][x]")
                    del_indexes = []

                    for x in range(len(root[splitted_path[0]])):
                        if (eval(filter)):
                            del_indexes.append(x)

                    # Sort the indexes in descending order to prevent index shifting
                    del_indexes.sort(reverse=True)

                    for i in del_indexes:
                        del root[splitted_path[0]][i]

                    return
                else:
                    filter = filter.replace("arg1", "root[splitted_path[0]]")
                    if (eval(filter)):
                        del root[splitted_path[0]]
                    return
        else:
            try:
                new_root = root.get(splitted_path[0])
            except  KeyError:
                raise KeyError(f'node \"{splitted_path[0]}\" not found') from None

            if isinstance(new_root, list):
                for x in new_root:
                    Converter.__delete(x, splitted_path[1:], filter)
            else:
                Converter.__delete(new_root, splitted_path[1:], filter)

    @staticmethod
    def __get(root, splitted_path, filter):
        '''
          Gets node from dict.
          Args:
           root (dict): Root of the dictionary.
           splitted_path (list): List of the following nodes (path to node).
           filter (str): Code used as additional condition for node to be extracted.
        '''

        #if we are at the end of path_to_node
        if len(splitted_path) == 1:

            if filter == None:
                #if there is no filter, just return what you got
                res = root[splitted_path[0]]
                return res
            else:
                #save result
                res = root[splitted_path[0]]
                filter = filter.replace("arg1", "")

                #if result contains more than one entry, run evaluate string on it to throw unnecesary ones
                if isinstance(res,list):
                    res2 = []
                    for x in res:
                        if(eval(f'x{filter}')):
                            res2.append(x)
                    return res2

                elif isinstance(res,(dict, str, int, float)):
                    if (eval(f'res{filter}')):
                        return res
                    return None
                else:
                    raise Exception(f'Not supported node type: {type(res)}')

        #if there is something more in path_to_node
        try:
            new_root = root[splitted_path[0]]
        except  KeyError:
            raise KeyError(f'node \"{splitted_path[0]}\" not found') from None
        return Converter.__get(new_root, splitted_path[1:], filter)

    @staticmethod
    def __set(root, splitted_path, value):
        '''
          Sets value in dict.
          Args:
           root (dict): Root of the dictionary.
           splitted_path (list): List of the following nodes (path to node).
           value: Value to be set. Could be literal, list or dict.
        '''

        if len(splitted_path) == 1:
            root[splitted_path[0]] = value
            return

        try:
            if root[splitted_path[0]] is None:
                root[splitted_path[0]] = dict()
            new_root = root[splitted_path[0]]
        except  KeyError:
            root.setdefault(splitted_path[0], dict())
            new_root = root[splitted_path[0]]

        return Converter.__set(new_root, splitted_path[1:], value)

    @staticmethod
    def convert(yaml_dict):
        '''
          Main method used to convert yaml using mappings.
          NOTE: Mappings are read from mappings package.

          Args:
           yaml_dict (dict): Dictionary to be converted.

        '''
        for mapping in mappings:
            mapping = tuple(s.lower() for s in mapping)

            print(mapping)
            if mapping[0] == 'add':
                path = mapping[1]
                try:
                    value = mapping[2]
                except IndexError:
                    value = None

                Converter.__set(yaml_dict, path.split('/'), value)

            elif mapping[0] == 'delete':
                path = mapping[1]
                try:
                    filter = mapping[2]
                except IndexError:
                    filter = None

                Converter.__delete(yaml_dict, path.split('/'), filter)

            elif mapping[0] == 'move':
                path1 = mapping[1]
                path2 = mapping[2]
                try:
                    filter = mapping[3]
                except IndexError:
                    filter = None

                value = Converter.__get(yaml_dict, path1.split('/'), filter)
                Converter.__delete(yaml_dict, path1.split('/'), filter)
                Converter.__set(yaml_dict, path2.split('/'), value)
            else:
                message = f'MAPPING: {mapping} COULD NOT BE PROCESSED. COMMAND UNRECOGNISED.'
                raise RuntimeError(message)
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

    parser.add_argument("-i", "--in-place", help="Input is renamed as <input>_old. Output is saved under input\'s filename.", action='store_true')
    args = parser.parse_args()

    if args.output and args.in_place:
        message = '\n-o/--output and -i/--in-place arguments cannot be used together. Output can be saved under input\'s filename or under new one, not both.'
        raise IllegalArgumentError(message)

    input_filename = args.file

    # ------- LOAD INPUT DATA -------
    with open(input_filename, 'r') as file:
        try:
            data = file.read()
        except FileNotFoundError as e:
            print(e)
            exit(0)

    yaml_dict = yaml.load(data, Loader=yaml.Loader)

    result_dict = Converter.convert(yaml_dict)

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