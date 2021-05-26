from iwrap.common.misc import Dictionarizable
import subprocess


class PkgConfigTools:
    PKG_CONFIG_CMD = "pkg-config"
    PKG_CONFIG_OPT_LIST_ALL = "--list-all"
    PKG_CONFIG_OPT_LIBS = "--libs"
    PKG_CONFIG_OPT_CFLAGS = "--cflags"

    def __init__(self):
        self.pkg_config_list = None
        self.initialize()
        self.to_dict()

    def initialize(self):
        process = subprocess.Popen([PkgConfigTools.PKG_CONFIG_CMD,
                                    PkgConfigTools.PKG_CONFIG_OPT_LIST_ALL,
                                    PkgConfigTools.PKG_CONFIG_OPT_LIBS,
                                    PkgConfigTools.PKG_CONFIG_OPT_CFLAGS], stdout=subprocess.PIPE)
        stdout, stderr = process.communicate()
        self.pkg_config_list = stdout.decode('ascii').splitlines()

    def to_dict(self):
        pkg_config_dict = {}
        for pkg_config in self.pkg_config_list:
            split_config = pkg_config.split(' ', 1)
            name = split_config[0]
            full_description = split_config[1].strip().split(' - ')
            info = full_description[0]
            desc = full_description[1]
            pkg_config_dict[name] = {'info': info, 'description': desc}

        return pkg_config_dict
