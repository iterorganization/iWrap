import os

IWRAP_DIR = os.path.dirname(os.path.realpath(__file__))

try:
    from importlib.metadata import version, PackageNotFoundError
except ImportError:
    # Python < 3.8
    from importlib_metadata import version, PackageNotFoundError

try:
    __version__ = version("iwrap")
except PackageNotFoundError:
    # Package is not installed
    __version__ = "unknown"
