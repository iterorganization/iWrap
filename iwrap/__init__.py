import os

IWRAP_DIR = os.path.dirname(os.path.realpath(__file__))

from . import _version
__version__ = _version.get_versions()['version']
