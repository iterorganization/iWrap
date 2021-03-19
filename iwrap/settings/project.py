from iwrap.common.misc import Dictionarizable, classproperty
from iwrap.settings.code_description import CodeDescription
from iwrap.settings.serialization import IWrapSerializer


class ProjectSettings( Dictionarizable ):
    _settings = None

    @classmethod
    def settings(cls):
        if cls._settings is None:
            cls._settings = ProjectSettings()

        return cls._settings

    def __init__(self):
        self.name: str = ''
        self.data_type: str = ''
        self.actor_type = ''
        self.code_description = CodeDescription()

    def from_dict(self, dictionary: dict) -> None:
        super().from_dict( dictionary )

    def to_dict(self):
        return super().to_dict()

    def clear(self):
        self.__init__()

    def save(self, serializer: IWrapSerializer):
        dictionary = self.to_dict()
        serializer.save( dictionary )

    def load(self, serializer: IWrapSerializer):
        dictionary = serializer.load()
        self.from_dict( dictionary )
