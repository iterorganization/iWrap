import logging
from typing import Set

from . import IDSConverter
from .legacy_storage import LegacyIDSStorage

from ...common.definitions import Argument
from ...common.runtime_settings import IdsStorageSettings
from .data_descriptions import IDSDescription


class LegacyIDSConverter(IDSConverter):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    def __init__(self):
        self.__data_storage = LegacyIDSStorage()

    def initialize(self, sandbox_dir: str, is_standalone: bool, storage_settings: IdsStorageSettings) -> None:

        db_name = storage_settings.db_name
        if is_standalone:
            backend_id = storage_settings.persistent_backend
        else:
            backend_id = storage_settings.backend
        self.__data_storage.initialize(sandbox_dir, db_name,  backend_id)

    def finalize(self) -> None:
        self.__data_storage.finalize()

    def data_type(self) -> str:
        return 'legacy'

    @classmethod
    def code_languages(cls) -> Set[str]:
        return {'cpp', 'fortran', 'java'}


    @staticmethod
    def __check(ids_name, ids_object):

        if ids_name != ids_object.__name__:
            raise RuntimeError("ERROR! Argument passed doesn't match arguments list: ", ids_name, ' vs ',
                               ids_object.__name__)

    def prepare_native_type(self, ids_description_class, ids_name):
        ids_description: IDSDescription = self.__data_storage.prepare_data(ids_name)

        return ids_description_class(ids_description)

    def convert_to_native_type(self, ids_description: IDSDescription, ids_intent, ids_object):

        # store input data
        if ids_intent == Argument.IN:
            self.__check(ids_description.ids_type, ids_object)
            self.__data_storage.save_data(ids_description, ids_object)

        return ids_description.convert_to_native_type()

    def convert_to_actor_type(self, ids_description: IDSDescription):
        ids = self.__data_storage.read_data( ids_description )
        return ids

    def release(self, ids_description: IDSDescription):
        self.__data_storage.release_data(ids_description.ids_type)

