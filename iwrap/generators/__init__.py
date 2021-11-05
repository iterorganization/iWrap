import logging
import sys
from abc import ABC, abstractmethod


class AbstractGenerator( ABC ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def configure(self, info_output_stream=sys.stdout):
        self.__info_output_stream = info_output_stream

    @abstractmethod
    def initialize(self, **kwargs):
        ...

    @abstractmethod
    def generate(self, actor_settings: dict):
        ...

    @abstractmethod
    def build(self):
        ...

    @abstractmethod
    def install(self):
        ...

    @abstractmethod
    def cleanup(self):
        ...
