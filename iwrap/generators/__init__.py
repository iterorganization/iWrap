import logging
import sys
from abc import ABC, abstractmethod


class AbstractGenerator( ABC ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    @abstractmethod
    def initialize(self, actor_settings: dict):
        ...

    @abstractmethod
    def generate(self, actor_settings: dict):
        ...

    @abstractmethod
    def build(self, actor_settings: dict):
        ...

    @abstractmethod
    def install(self, actor_settings: dict):
        ...

    @abstractmethod
    def cleanup(self, actor_settings: dict):
        ...
