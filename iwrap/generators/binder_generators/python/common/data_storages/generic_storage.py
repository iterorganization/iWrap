import os
from abc import ABC, abstractmethod
from pathlib import Path

import imas

from ..data_c_binding import IDSCType


class GenericIDSStorage(ABC):

    @abstractmethod
    def prepare_data(self, ids_name):
        ...

    @abstractmethod
    def save_data(self, ids_name, ids_data):
        ...

    @abstractmethod
    def read_data(self, ids_name, ids_data):
        ...

    def initialize(self, db_name:str, backend_id):
        ...


class LegacyIDSStorage( GenericIDSStorage ):

    def __init__(self):
        self.__occ_dict = {}
        self.__db_entry = None

    def __get_occurrence(self, ids_name):
        occ = 1 + self.__occ_dict.get(ids_name, -1 )
        self.__occ_dict[ids_name] = occ
        return occ

    def __release_occurrence(self, ids_name):
        occ = self.__occ_dict.get(ids_name, 1 )
        self.__occ_dict[ids_name] = occ - 1

    def __create_cache_db(cls, sandbox_dir:str, db_name:str, backend_id, shot, run):

        Path(sandbox_dir, 'tmp', '3', '0').mkdir(parents=True, exist_ok=True)

        db_entry = imas.DBEntry( backend_id=backend_id,   # pylint: disable=no-member
                                 user_name=sandbox_dir,   # AL hack to use sandbox dir
                                 db_name=db_name,
                                 shot=shot,
                                 run=run )

        status, _not_used = db_entry.create()
        if status != 0:
            raise Exception(f"Error creating the temporary DB: backend={backend_id} name={db_name} shot={shot} run={run} dir={sandbox_dir}")

        return db_entry

    def initialize(self, sandbox_dir: str, db_name:str, backend_id):
        shot = 1
        run = 1
        self.__db_entry =  self.__create_cache_db(sandbox_dir, db_name, backend_id, shot, run)

    def prepare_data(self, ids_name):
        occurrence = self.__get_occurrence( ids_name )

        ids_description = IDSCType(self.__db_entry, ids_name, occurrence)
        return ids_description

    def save_data(self, ids_description:IDSCType, legacy_ids):
        self.__db_entry.put( legacy_ids, ids_description.occurrence )

    def read_data(self, ids_description:IDSCType):
        legacy_ids = self.__db_entry.get( ids_description.ids_name, ids_description.occurrence )
        return legacy_ids

    def release_data(self, ids_name):
        self.__release_occurrence( ids_name )

    def finalize(self):
        try:
            self.__db_entry.close(erase=True)
        except:
            self.__db_entry.close()
