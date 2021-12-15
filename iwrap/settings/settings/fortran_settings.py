import logging
from abc import ABC
from pathlib import Path
from typing import Dict, Any

from iwrap.common import utils
from iwrap.generation_engine.engine import Engine
from iwrap.settings import SettingsBaseClass


class AbstractLanguageSpecificSettings( SettingsBaseClass, ABC ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


class ExtraLibraries( SettingsBaseClass ):
    """ Class for libraries from path and libraries defines by pkg config.
        Attributes:
            pkg_config_defined (list [`str`]): list of pkg config defined libraries.
            path_defined (list [`str`]): list of path defined libraries.
    """
    def __init__(self):
        self.pkg_config_defined = []
        self.path_defined = []

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # system_libraries
        # TODO Validate system libs against platform settings

        # custom_libraries
        for library in self.path_defined or []:
            __path = utils.resolve_path(library, project_root_dir)
            if not Path(__path).exists():
                raise ValueError( f'Path to library file is not valid! {str( __path )}' )

    def clear(self):
        """Clears class content."""
        self.__init__()

    def from_dict(self, dictionary: dict):
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative:str = False, project_root_dir:str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            resolved_libs = []
            for library in self.path_defined or []:
                __path = utils.resolve_path( library, project_root_dir )
                resolved_libs.append(__path)
            ret_dict.update( {'path_defined': resolved_libs} )

        return ret_dict


class FortranSpecificSettings( AbstractLanguageSpecificSettings ):
    """ The fortran language specific settings.
    Attributes:
        compiler_cmd (str): the compiler command used to compile native codes.
        _open_mp_switch (str): the OpenMP switch.
        _mpi_compiler_cmd (str): the MPI compiler command
        extra_libraries (:obj:`ExtraLibraries`): extra libraries defined by paths or pkg configs.
    """
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    @property
    def open_mp_switch(self):
        return self._open_mp_switch

    @open_mp_switch.setter
    def open_mp_switch(self, value):
        self._open_mp_switch = value if value != "None" else None

    @property
    def mpi_compiler_cmd(self):
        return self._mpi_compiler_cmd

    @mpi_compiler_cmd.setter
    def mpi_compiler_cmd(self, value):
        self._mpi_compiler_cmd = value if value != "None" else None

    def __init__(self):
        self.compiler_cmd = ''
        self._open_mp_switch = False
        self._mpi_compiler_cmd = ''
        self.extra_libraries = ExtraLibraries()

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # compiler_cmd
        if not self.compiler_cmd:
            raise ValueError( 'Compiler to be used is not set!' )
        # TODO Validate compiler against platform settings

        # open_mp
        # TODO validate open mp

        # extra_libraries
        self.extra_libraries.validate(engine, project_root_dir)

    def clear(self):
        """Clears class content
        """
        self.__init__()

    def from_dict(self, dictionary: dict):
        """Restores given object from dictionary.

           Args:
               dictionary (Dict[str], Any): Data to be used to restore object
           """
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative:str = False, project_root_dir:str = None) -> Dict[str, Any]:
        """Serializes given object to dictionary

        Returns
            Dict[str, Any]: Dictionary containing object data
        """
        return super().to_dict()

