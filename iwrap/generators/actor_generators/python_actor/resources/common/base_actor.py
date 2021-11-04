import os
import logging
from abc import ABC

from .runtime_settings import RuntimeSettings
from ..binding.binder import CBinder


class ActorBaseClass( ABC ):
    # Class logger
    __logger = logging.getLogger(__name__ + "." + __qualname__)

    __instance_index = 0

    @property
    def unique_id(self):
        class_name = self.__class__.__name__
        idx = self.__instance_index
        pid = os.getpid()
        uid = f'{class_name}_{idx}#{pid}'
        return uid

    def __new__(cls):
        new_object = object.__new__( cls )
        cls.__instance_index += 1
        return new_object

    def __init__(self):
        self.runtime_settings = RuntimeSettings()
        self.arguments = []
        self.code_parameters = None

        self.name = self.__class__.__name__

        self.__binder = CBinder( )


    # # #  Actor lifecycle methods # # #

    def initialize(self):

        if self.code_parameters:
            self.code_parameters.initialize()

        self.__binder.initialize(actor=self)




    def __call__(self, *args):
        return self.run( *args )

    def run(self, *args):
        out = self.__binder.step( *args )
        return out

    def finalize(self):
        self.__binder.finalize()
