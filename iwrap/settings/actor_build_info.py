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
        self.al_version = getenv('AL_VERSION') or getenv('UAL_VERSION')
        self.generation_date = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
