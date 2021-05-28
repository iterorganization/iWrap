import subprocess


class PkgConfigTools:
    """The class enables to get a dictionary with all pkg configs and get info and description values for the system library.

    Attributes:
        pkg_config_list (list): The list contains pkg configs.
        PKG_CONFIG_CMD (str): The config command.
        PKG_CONFIG_OPT_LIST_ALL (str): The command option enables to list all configs.
    """
    PKG_CONFIG_CMD = "pkg-config"
    PKG_CONFIG_OPT_LIST_ALL = "--list-all"

    def __init__(self):
        """Initialize the PkgConfigTools class object.
        """
        self.pkg_config_list = None
        self.initialize()

    def initialize(self):
        """Get list of pkg configs.
        """
        process = subprocess.Popen([PkgConfigTools.PKG_CONFIG_CMD,
                                    PkgConfigTools.PKG_CONFIG_OPT_LIST_ALL], stdout=subprocess.PIPE)
        stdout, stderr = process.communicate()
        self.pkg_config_list = stdout.decode('ascii').splitlines()

    def to_dict(self):
        """Return dictionary with all pkg configs.

        Returns (dict): The dictionary with all pkg configs. Keys are pkg configs names, dictionary values are
        info and descriptions.
        """
        pkg_config_dict = {}
        for pkg_config in self.pkg_config_list:
            split_config = pkg_config.split(' ', 1)
            name = split_config[0]
            full_description = split_config[1].strip().split(' - ')
            info = full_description[0]
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
        pkg_config_dict = self.to_dict()
        if system_library in pkg_config_dict.keys():
            return pkg_config_dict[system_library]
        return None
