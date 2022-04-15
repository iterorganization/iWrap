import logging
import ctypes
from typing import Set

import imas

from .data_c_binding import IDSCType
from .data_storages import IDSConverter
from .data_storages.generic_storage import LegacyIDSStorage

from ..common.definitions import Argument
from ..common.runtime_settings import IdsStorageSettings


class LegacyIDSConverter( IDSConverter, ctypes.Structure ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        self.__data_storage = LegacyIDSStorage()

    def initialize(self, actor_unique_id: str, is_standalone: bool, storage_settings: IdsStorageSettings) -> None:

        db_name = storage_settings.db_name
        if is_standalone:
            backend_id = storage_settings.persistent_backend
        else:
            backend_id = storage_settings.backend
        self.__data_storage.initialize(actor_unique_id, db_name,  backend_id)

    def finalize(self) -> None:
        self.__data_storage.finalize()

    def data_type(self) -> str:
        return 'legacy'

    @classmethod
    def code_languages(cls) -> Set[str]:
        return {'cpp', 'fortran'}


    @staticmethod
    def __check(ids_name, ids_object):

        if ids_name != ids_object.__name__:
            raise RuntimeError("ERROR! Argument passed doesn't match arguments list: ", ids_name, ' vs ',
                               ids_object.__name__)

    def prepare_native_type(self, ids_name):
        ids_description: IDSCType = self.__data_storage.prepare_data(ids_name)

        return ids_description

    def convert_to_native_type(self, ids_description: IDSCType, ids_intent, ids_object):

        # store input data
        if ids_intent == Argument.IN:
            self.__check(ids_description.ids_name, ids_object)
            self.__data_storage.save_data(ids_description, ids_object)

        return ctypes.byref( ids_description )

    def convert_to_actor_type(self, ids_description: IDSCType):
        ids = self.__data_storage.read_data( ids_description )
        return ids

    def release(self, ids_description: IDSCType):
        self.__data_storage.release_data(ids_description.ids_name)

