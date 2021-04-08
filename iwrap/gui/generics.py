from abc import ABC, abstractmethod


class IWrapPane(ABC):

    @abstractmethod
    def reload(self):
        ...


    @abstractmethod
    def update_settings(self):
        ...

