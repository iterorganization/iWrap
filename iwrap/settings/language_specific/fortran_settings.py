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

    pass



class MPI( SettingsBaseClass ):

    @property
    def mpi_compiler_cmd(self):
        return self._mpi_compiler_cmd

    @mpi_compiler_cmd.setter
    def mpi_compiler_cmd(self, value):
        self._mpi_compiler_cmd = value if value != "None" else None

    @property
    def mpi_runner(self):
        return self._mpi_runner

    @mpi_runner.setter
    def mpi_runner(self, value):
        self._mpi_runner = value if value != "None" else None

    def __init__(self):
        self._mpi_compiler_cmd = ''
        self._mpi_runner = ''

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # mpi_compiler_cmd
        # mpi_runner
        if self.mpi_runner and not self.mpi_compiler_cmd:
            raise ValueError('MPI compiler command is not set')

        if self.mpi_compiler_cmd and not self.mpi_runner:
            raise ValueError('MPI runner is not set')

    def clear(self):
        self.__init__()

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()


class ExtraLibraries( SettingsBaseClass ):

    def __init__(self):
        self.pkg_config_defined = []
        self.path_defined = []

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # system_libraries
        # TODO Validate system libs against platform settings

        # custom_libraries
        for library in self.path_defined or []:
            __path = utils.resolve_path(library, project_root_dir)
            if not __path.exists():
                raise ValueError( f'Path to library file is not valid! {str( __path )}' )

    def clear(self):
        self.__init__()

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative:str = False, project_root_dir:str = None) -> Dict[str, Any]:
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            # include_path
            resolved_libs = []
            for library in self.path_defined or []:
                __path = utils.resolve_path( library, project_root_dir )
                resolved_libs.append(__path)
            ret_dict.update( {'path_defined': resolved_libs} )

        return ret_dict


class FortranSpecificSettings( AbstractLanguageSpecificSettings ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    @property
    def open_mp_switch(self):
        return self._open_mp_switch

    @open_mp_switch.setter
    def open_mp_switch(self, value):
        self._open_mp_switch = value if value != "None" else None

    def __init__(self):
        self.compiler_cmd = ''
        self.include_path = ''
        self.mpi = MPI()
        self._open_mp_switch = False
        self.extra_libraries = ExtraLibraries()

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # compiler_cmd
        if not self.compiler_cmd:
            raise ValueError( 'Compiler to be used is not set!' )
        # TODO Validate compiler against platform settings

        # include_path
        if not self.include_path:
            raise ValueError( 'Path to include/module file is not set!' )

        __path = utils.resolve_path( self.include_path, project_root_dir)
        if not Path(__path).exists():
            raise ValueError( f'Path to include/module file is not valid! {str( __path )}' )

        # mpi
        self.mpi.validate(engine, project_root_dir)

        # open_mp
        # TODO validate open mp

        # extra_libraries
        self.extra_libraries.validate(engine, project_root_dir)

    def clear(self):
        self.__init__()

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self, resolve_path: bool = False, make_relative:str = False, project_root_dir:str = None) -> Dict[str, Any]:
        ret_dict = super().to_dict(resolve_path, make_relative, project_root_dir)

        if resolve_path:
            # include_path
            __path = utils.resolve_path( self.include_path, project_root_dir )
            ret_dict.update( {'include_path': __path} )

        return ret_dict

    pass
