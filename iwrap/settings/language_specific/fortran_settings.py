from abc import ABC
from pathlib import Path

from iwrap.generation_engine.engine import Engine
from iwrap.settings import SettingsBaseClass


class AbstractLanguageSpecificSettings( SettingsBaseClass, ABC ):
    pass


class FortranSpecificSettings( AbstractLanguageSpecificSettings ):

    @property
    def mpi(self):
        if not self._mpi:
            return None
        else:
            return self._mpi

    @mpi.setter
    def mpi(self, value):
        if value == 'None':
            self._mpi = False
        else:
            self._mpi = value

    def __init__(self):
        self.compiler = ''
        self.include_path = ''
        self._mpi = ''
        self.open_mp = False
        self.system_libraries = []
        self.custom_libraries = []

    def validate(self, engine: Engine, project_root_dir: str) -> None:

        # compiler
        if not self.compiler:
            raise ValueError( 'Compiler to be used is not set!' )
        # TODO Validate compiler against platform settings

        # include_path
        if not self.include_path:
            raise ValueError( 'Path to include/module file is not set!' )

        __path = Path( project_root_dir, self.include_path )
        if not __path.exists():
            raise ValueError( f'Path to include/module file is not valid! {str(path)}' )

        # mpi
        # TODO Validate MPI against platform settings

        # open_mp
        # TODO validate open mp

        # system_libraries
        # TODO Validate system libs against platform settings

        # custom_libraries
        for library in self.custom_libraries or []:
            __path = Path( project_root_dir, library)
            if not __path.exists():
                raise ValueError( f'Path to library file is not valid! {str(__path)}' )

    def clear(self):
        self.__init__()

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()

    pass
