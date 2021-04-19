from iwrap.common.misc import Dictionarizable


class FortranSpecificSettings(Dictionarizable):

    def __init__(self):
        self.compiler = ''
        self.mpi = ''
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