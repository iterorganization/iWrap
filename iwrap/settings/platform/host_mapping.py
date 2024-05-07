import logging
import socket
from pathlib import Path

import iwrap

__logger = logging.getLogger( __name__ )

known_hosts = {
    "marconi.cineca.it" : "EUROfusion.Marconi",
    "galileo.cineca.it" : "EUROfusion.Marconi"
}


def get_config_file_for_domain():

    domain_fqdn: str = socket.getfqdn()

    for key, value in known_hosts.items():
        if key in domain_fqdn:
            config_file_name = f'config_{value}.yaml'
            break
    else:
        config_file_name = "config_default.yaml"
        __logger.warning( f'No specific config for "{domain_fqdn}". Default config file is used.' )

    config_file_path = Path(iwrap.IWRAP_DIR, 'resources/config', config_file_name)

    if not config_file_path.exists():
        __logger.warning( f'File "{config_file_name}" not found. Default config file is used.' )
        config_file_path = Path( iwrap.IWRAP_DIR, 'resources/config', "config_default.yaml" )

    return config_file_path


if __name__ == "__main__":
    file_path = get_config_file_for_domain()
    print(file_path)
