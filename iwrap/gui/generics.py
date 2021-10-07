import logging
from abc import ABC, abstractmethod


class IWrapPane(ABC):
    # Class logger
    logger = logging.getLogger(__name__ + "." + __qualname__)


    @abstractmethod
    def reload(self):
        ...


    @abstractmethod
    def update_settings(self):
        ...

