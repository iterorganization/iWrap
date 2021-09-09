from abc import ABC
from pathlib import Path

from iwrap.common.misc import Dictionarizable
from iwrap.generation_engine.engine import Engine


# TODO Add a superclass for language handlers
class AbstractLanguageSpecificSettings( ABC ):
    pass


class FortranSpecificSettings( Dictionarizable, AbstractLanguageSpecificSettings ):

    @property
    def mpi(self):
        if not self._mpi:
            return None
        else:
            return self._mpi

    @mpi.setter
    def mpi(self, value):
        if value == None:
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

        if not Path( project_root_dir, self.include_path ).exists():
            raise ValueError( 'Path to include/module file is not valid!' )

        # mpi
        # TODO Validate MPI against platform settings

        # open_mp
        # TODO validate open mp

        # system_libraries
        # TODO Validate system libs against platform settings

        # custom_libraries
        for library in self.custom_libraries or []:
            if not Path( project_root_dir, library ).exists():
                raise ValueError( 'Path to library file is not valid!' )

    def clear(self):
        self.__init__()

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()

    pass
