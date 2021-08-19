from iwrap.common.misc import Dictionarizable


class FortranSpecificSettings(Dictionarizable):

    @property
    def mpi(self):
        if not self._mpi:
            return 'None'
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

    def clear(self):
        self.__init__()

    def from_dict(self, dictionary: dict):
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()

    pass