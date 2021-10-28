import logging
from pathlib import Path
from typing import Dict, Any

import iwrap
import yaml
from iwrap.common import utils

from iwrap.common.misc import Dictionarizable


class DefaultDirectories(Dictionarizable):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    def __init__(self):
        self.actor_default_install_dir = str(Path(Path.home(), 'IWRAP_ACTORS'))
        self.sandbox_default_dir: str = str(Path(Path.home(), 'IWRAP_SANDBOX'))

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict(dictionary)

        if self.actor_default_install_dir:
            self.actor_default_install_dir = utils.resolve_path(self.actor_default_install_dir)

        if self.sandbox_default_dir:
            self.sandbox_default_dir = utils.resolve_path(self.sandbox_default_dir)

    def to_dict(self, resolve_path: bool = False, make_relative: str = False,
                project_root: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class PlatformSettings(Dictionarizable):
    """ TO DO: Read a proper content from config file
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    __class_instance = None
    __was_inited = False

    def __new__(cls, *args, **kwargs):
        if cls.__class_instance is None:
            cls.__class_instance = super().__new__(cls, *args, **kwargs)
        return cls.__class_instance

    def __init__(self):
        if self.__was_inited:
            return

        self.default_directories = DefaultDirectories()
        self.__was_inited = True

    def __read_config_file(self):
        env = {'IMAS_CONFIG_PREFIX': 'config_', 'IMAS_CONFIG_SUFFIX': '.yaml'}
        config_file_dir = Path(iwrap.IWRAP_DIR, 'resources')
        file_name = utils.exec_system_cmd('imas-config-fc2k',
                                          working_directory=config_file_dir,
                                          environment=env,
                                          return_output=True)
        file_path = Path(config_file_dir, file_name)
        self.__logger.debug(f'Configuration read from: {file_path}')
        self.load(file_path)
        pass

    def initialize(self):
        self.__read_config_file()
        pass

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict(dictionary)

    def to_dict(self, resolve_path: bool = False, make_relative: str = False,
                project_root_dir: str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

    def save(self, stream):
        """Stores code description in a file

        Args:
            stream : an object responsible for storing data
        """

        dict_to_store = {'platform_settings': self.to_dict()}
        yaml.dump(dict_to_store, stream=stream, default_flow_style=False, sort_keys=False, indent=4,
                  explicit_start=True, explicit_end=True)

    def load(self, file_path):
        """Loads code description from a file

        Args:
            file_path (Path): a path to file containing YAML
        """

        with open(file_path) as yaml_file:
            read_dict = yaml.load(yaml_file, Loader=yaml.Loader)

        if not read_dict:
            raise Exception(f"The file being loaded doesn't seem to be a valid YAML: {file_path}")

        platform_settings_dict = read_dict.get('platform_settings')

        if not platform_settings_dict:
            raise Exception(f"The YAML file being loaded doesn't seem to contain platform properties: {file_path}")

        self.from_dict(platform_settings_dict)


if __name__ == "__main__":
    settings = PlatformSettings()
    settings.initialize()
    import sys
    sys.exit()

    '''
    print(os.path.realpath(os.curdir))
    file = open("./platf.yaml", '+w')
    settings.save(file)
    file.close()
    del settings
    '''
    settings = PlatformSettings()
    file = open("./platf.yaml", '+r')
    settings.load(file)
    file.close()
