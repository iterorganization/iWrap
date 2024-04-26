import logging
from pathlib import Path
from typing import Dict, Any

from iwrap.settings.platform.host_mapping import get_config_file_for_domain

import iwrap
import yaml
from iwrap.common import utils

from iwrap.common.misc import Dictionarizable


class DefaultDirectories(Dictionarizable):

    def __init__(self):
        self.actor_install_dir = str(Path(Path.home(), 'IWRAP_ACTORS'))
        self.sandbox_dir: str = str(Path(Path.home(), 'IWRAP_SANDBOX'))

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict(dictionary)

        if self.actor_install_dir:
            self.actor_install_dir = utils.resolve_path(self.actor_install_dir)


class BatchJobs(Dictionarizable):

    def __init__(self):
        self.runner: str = ''
        self.options: str = ''

class MPIJobs(Dictionarizable):

    def __init__(self):
        self.runner: str = ''
        self.options: str = ''


class Debugger(Dictionarizable):

    def __init__(self):
        self.cmd: str = ''
        self.attach_cmd: str = ''


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

        self.directories = DefaultDirectories()
        self.batch_jobs = BatchJobs()
        self.mpi_jobs = MPIJobs()
        self.debugger = Debugger()
        self.__was_inited = True

    def __read_config_file(self):

        file_path = get_config_file_for_domain()
        self.__logger.info( f'Configuration read from: {file_path}' )
        self.load(file_path)
        pass

    def initialize(self):
        self.__read_config_file()
        pass

    def save(self, stream):
        """Stores code description in a file

        Args:
            stream : an object responsible for storing data
        """

        dict_to_store = {'platform_settings': self.to_dict()}
        yaml.dump(dict_to_store, stream=stream, flow_style=False, sort_keys=False, indent=4,
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
