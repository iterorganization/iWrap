from iwrap.common.misc import Dictionarizable
from typing import List, Any, Dict

from datetime import datetime
from iwrap import __version__
from os import getenv

class ActorBuildInfo(Dictionarizable):
    def __init__(self):
        self.iwrap_version = __version__
        self.imas_version = getenv('IMAS_VERSION')
        self.imas_prefix = getenv('IMAS_PREFIX')
        self.al_version = getenv('UAL_VERSION')
        self.generation_date = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        self.iwrap_version = dictionary.get('iwrap_version', 'not specified')
        self.imas_version = dictionary.get('imas_version', 'not specified')
        self.imas_prefix = dictionary.get('imas_prefix', 'not specified')
        self.imas_prefix = dictionary.get('al_version', 'not specified')
        self.generation_date = dictionary.get('generation_date', 'not specified')

    def to_dict(self, resolve_path: bool = False, make_relative: str = False, project_root_dir: str = None) -> Dict[str, Any]:
        dict = {
            'iwrap_version': self.iwrap_version,
            'imas_version': self.imas_version,
            'imas_prefix': self.imas_prefix,
            'al_version': self.al_version,
            'generation_date': self.generation_date
        }
        return dict