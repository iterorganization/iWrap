from abc import ABC, abstractmethod
from iwrap.common.misc import Dictionarizable
from iwrap.generation_engine.engine import Engine


class SettingsBaseClass( Dictionarizable, ABC):

    @abstractmethod
    def clear(self):
        ...

    @abstractmethod
    def validate(self, engine: Engine, project_root_dir: str, **kwargs) -> None:
        ...
