import logging
import inspect
from abc import ABC, abstractmethod
from typing import List, Any, Dict

import yaml


class Dictionarizable( ABC ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def _list_attributes(self):
        members_list = inspect.getmembers(self,  lambda member: not inspect.isroutine(member))
        members_list = filter( lambda member: not member[0].startswith( "_" ), members_list )

        return members_list

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        if type(dictionary) is not dict:
            error_msg = f'ERROR: Incorrect type of "{dictionary}" field! Configuration seems to be not a valid iWrap file!'
            raise ValueError(error_msg)

        for name, value in dictionary.items():
            if not hasattr( self, name ):
                error_msg = f'ERROR: Unknown field "{name}". Configuration seems to be not a valid iWrap file!'
                raise ValueError(error_msg)
            attr = getattr( self, name )
            if isinstance( attr, Dictionarizable ):
                if value:
                    attr.from_dict( value )
            else:
                if str(value).lower() == 'none' or value == '' or str(value).lower() == 'null':
                    value = None
                setattr( self, name, value )

    def to_dict(self, resolve_path: bool = False, make_relative:str = False, project_root_dir:str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Args:
            resolve_path (bool): Determines if paths with system variables should be 'expanded' or left as they are
            project_root_dir (str): The root of all relative paths

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        obj_as_dict_ = self._list_attributes()
        dict_ = {}
        for key, value in self._list_attributes():
            if isinstance( value, Dictionarizable ):
                dict_[key] = value.to_dict(resolve_path, make_relative, project_root_dir)
            elif isinstance( value, List ):
                dict_[key] = [item.to_dict(resolve_path, make_relative, project_root_dir) if isinstance( item, Dictionarizable ) else item for item in value]
            else:
                dict_[key] = value

        return dict_


class CustomDumper(yaml.SafeDumper):
    def ignore_aliases(self, data):
        return True

    #dump empty sequence as: ' ' (nothing)
    def represent_sequence(self, tag, sequence, flow_style=None):
        if not sequence:  # Check if the sequence is empty
            return super().represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_sequence(tag, sequence, flow_style)

    # dump empty dict as: ' ' (nothing)
    def represent_mapping(self, tag, mapping, flow_style=None):
        if not mapping:  # Check if the sequence is empty
            return super().represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_mapping(tag, mapping, flow_style)

    def represent_none(self, data):
        return super().represent_scalar('tag:yaml.org,2002:null', u'')

    # dump empty dict as: ' ' (nothing)
    def represent_scalar(self, tag, value, style=None):
        if not value or value.lower() == 'null':  # Check if value is None or 'null'
            return super().represent_scalar(u'tag:yaml.org,2002:null', u'')
        return super().represent_scalar(tag, value, style)
