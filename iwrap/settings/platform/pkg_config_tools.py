import logging
import subprocess


class PkgConfigTools:
    """The class enables to get a dictionary with all pkg configs and get info and description values for the system library.

    Properties:
        system_lib_dict (dict): The dictionary with all pkg configs. Keys are pkg configs names, dictionary values are
        info and descriptions.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    PKG_CONFIG_CMD = "pkg-config"
    """PKG_CONFIG_CMD (str): The config command."""
    PKG_CONFIG_OPT_LIST_ALL = "--list-all"
    """PKG_CONFIG_OPT_LIST_ALL (str): The command option enables to list all configs."""
    PKG_CONFIG_OPT_GET_LINKER_FLAGS = "--libs"
    PKG_CONFIG_OPT_GET_CFLAGS = "--cflags"

    def __init__(self):
        """Initialize the PkgConfigTools class object.
        """
        self.__pkg_config_list = None
        self.__system_lib_dict = None

    @property
    def system_lib_dict(self):
        """Get dictionary with pkg configs.

        Returns (dict): The dictionary with all pkg configs. Keys are pkg configs names, dictionary values are
        info and descriptions.
        """
        return self.__system_lib_dict

    def initialize(self):
        """Call subprocess and get a list of pkg configs"
        """
        try:
            process = subprocess.Popen([PkgConfigTools.PKG_CONFIG_CMD, PkgConfigTools.PKG_CONFIG_OPT_LIST_ALL],
                                       encoding='utf-8', text=True,  stdout=subprocess.PIPE)
            self.__pkg_config_list = process.stdout.readlines()
        except (FileNotFoundError, subprocess.CalledProcessError):
            self.__pkg_config_list = []
        self.__system_lib_dict = self.__to_dict()

    def __to_dict(self):
        pkg_config_dict = {}
        for pkg_config in self.__pkg_config_list:
            split_config = pkg_config.split(' ', 1)
            name = split_config[0]
            full_description = split_config[1].strip().split(' - ')
            info = full_description[0]
            desc = ''
            if len(full_description) > 1:
                desc = full_description[1]

            pkg_config_dict[name] = {'info': info, 'description': desc}

        return pkg_config_dict

    def get_pkg_config(self, system_library):
        """Returns dictionary with info and description values related to the system_library or None if the system
        library is not available.

        Args:
            system_library (str): The system library name.

        Returns (dict/None): The dictionary with info and description values or None if the system library
        is not available.
        """
        if system_library in self.system_lib_dict.keys():
            return self.system_lib_dict[system_library]
        return None

    def get_linker_flags(self, system_library):
        try:
            process = subprocess.Popen([PkgConfigTools.PKG_CONFIG_CMD,
                                        PkgConfigTools.PKG_CONFIG_OPT_GET_LINKER_FLAGS,
                                        system_library],
                                       encoding='utf-8', text=True,  stdout=subprocess.PIPE)
            linker_flags = "".join(process.stdout.readlines())
        except (FileNotFoundError, subprocess.CalledProcessError):
            linker_flags = ''

        return linker_flags

    def get_c_flags(self, system_library):
        try:
            process = subprocess.Popen([PkgConfigTools.PKG_CONFIG_CMD,
                                        PkgConfigTools.PKG_CONFIG_OPT_GET_CFLAGS,
                                        system_library],
                                       encoding='utf-8', text=True,  stdout=subprocess.PIPE)
            cflags_flags = "".join(process.stdout.readlines())
        except (FileNotFoundError, subprocess.CalledProcessError):
            cflags_flags = ''

        return cflags_flags
