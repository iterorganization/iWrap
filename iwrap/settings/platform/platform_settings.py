import logging
from pathlib import Path
from typing import Dict, Any

import yaml

from iwrap.common.misc import Dictionarizable


class Debugger( Dictionarizable ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self):
        self.name: str = ''
        self.cmd: str = ''
        self.attach_cmd: str = ''

        # self.debugger_switches = {}

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self) -> None:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class MPIFlavor( Dictionarizable ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self):
        self.flavor: str = ''
        self.description: str = ''
        self.compiler_cmd: str = ''
        self.launcher_cmd: str = ''

        # self.debugger_switches = {}

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self) -> None:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class Compiler( Dictionarizable ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self):
        self.description: str = ''
        self.cmd: str = ''
        self.compiler_flags: str = ''
        self.linker_flags: str = ''
        self.imas_compiler_flags: str = ''
        self.imas_linker_flags: str = ''
        self.open_mp_switch: str = ''
        self.mpi_flavors = []

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self) -> None:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class ProgrammingLanguage( Dictionarizable ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    def __init__(self):
        self.name: str = ''
        self.compilers = []

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self) -> None:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()


class PlatformSettings( Dictionarizable ):
    """ TO DO: Read a proper content from config file
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    __class_instance = None

    def __new__(cls):
        if cls.__class_instance is None:
            cls.__class_instance = object.__new__( cls )
        return cls.__class_instance

    def __init__(self):
        self.actor_default_dir = str( Path( Path.home(), 'IWRAP_ACTORS' ) )  # TODO: Read install dir from platform settings
        self.sandbox_path: str = ''
        self.programming_languages = []
        self.debuggers = []

    def initialize(self):
        # TODO: Load platform settings from file
        pass

    def from_dict(self, dictionary: Dict[str, Any]) -> None:
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self) -> None:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

    def save(self, stream):
        """Stores code description in a file

        Args:
            stream : an object responsible for storing data
        """

        yaml.dump( self.to_dict(), stream=stream, default_flow_style=False, sort_keys=False, indent=4,
                   explicit_start=True, explicit_end=True )

    def load(self, file):
        """Loads code description from a file

        Args:
            serializer (IWrapSerializer): an object responsible for reading dictionary from file of given format
        """

        read_dict = yaml.load( file, Loader=yaml.Loader )
        import pprint
        pprint.pprint(read_dict)
        self.from_dict(read_dict)


if __name__ == "__main__":
    settings = PlatformSettings()
    language = ProgrammingLanguage()
    language.name = 'Fortran'

    compiler = Compiler()
    compiler.mpi_flavors.append( MPIFlavor() )

    language.compilers.append( compiler )

    settings.programming_languages.append( language )

    settings.debuggers.append(Debugger())

    file = open( "./platf.yaml", '+w' )
    settings.save( file )
    file.close()
    del settings

    settings = PlatformSettings()
    file = open( "./platf-2.yaml", '+r' )
    settings.load( file )
    file.close()