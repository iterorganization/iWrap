import subprocess
import re


class PkgConfigTools:
    PKG_CONFIG_CMD = "pkg-config"
    PKG_CONFIG_OPT_LIST_ALL = "--list-all"
    PKG_CONFIG_OPT_LIBS = "--libs"
    PKG_CONFIG_OPT_CFLAGS = "--cflags"

    def __init__(self):
        self.pkg_config_list = None
        self.initialize()
        self.get_pkg_config_dict()

    def initialize(self):
        process = subprocess.Popen([PkgConfigTools.PKG_CONFIG_CMD,
                                    PkgConfigTools.PKG_CONFIG_OPT_LIST_ALL,
                                    PkgConfigTools.PKG_CONFIG_OPT_LIBS,
                                    PkgConfigTools.PKG_CONFIG_OPT_CFLAGS], stdout=subprocess.PIPE)
        stdout, stderr = process.communicate()
        self.pkg_config_list = stdout.decode('ascii').splitlines()

    def get_pkg_config_dict(self):
        for pkg_config in self.pkg_config_list:
            split_config = re.split(r'\s+', pkg_config)
            name = split_config[0]
            info = split_config[1]
            description = ' '.join(split_config[3:])
            print(split_config)
            print(f"name = {name}, info = {info}, desc = {description}")
