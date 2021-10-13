import logging
from abc import ABC, abstractmethod


class IWrapPane(ABC):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)


    @abstractmethod
    def reload(self):
        ...


    @abstractmethod
    def update_settings(self):
        ...

