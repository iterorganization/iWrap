import os
from abc import ABC, abstractmethod
from pathlib import Path

import imas

from .data_descriptions import IDSDescription
from .generic_storage import GenericIDSStorage


class LegacyIDSStorage( GenericIDSStorage ):

    def __init__(self):
        self.__occ_dict = {}
        self.__db_entry = None
        self.__backend_id: int = -1
        self.__db_name: str = ''
        self.__pulse: int = -1
        self.__run: int = -1
        self.__sandbox_dir: str = ''

    def __get_occurrence(self, ids_name):
        occ = 1 + self.__occ_dict.get(ids_name, -1 )
        self.__occ_dict[ids_name] = occ
        return occ

    def __release_occurrence(self, ids_name):
        occ = self.__occ_dict.get(ids_name, 1 )
        self.__occ_dict[ids_name] = occ - 1

    def __open_db(self):

        if  "ids_defs" in dir(imas) and self.__backend_id == imas.ids_defs.MEMORY_BACKEND:
            return
        if  "imasdef" in dir(imas) and self.__backend_id == imas.imasdef.MEMORY_BACKEND:
            return
        status, _not_used = self.__db_entry.open()
        if status != 0:
            raise Exception(
                f"Error opening the temporary DB: "
                f"backend={self.__backend_id} "
                f"name={self.__db_name} "
                f"pulse={self.__pulse} "
                f"run={self.__run} "
                f"dir={self.__sandbox_dir}" )

    def __close_db(self):

        if "ids_defs" in dir(imas) and self.__backend_id == imas.ids_defs.MEMORY_BACKEND:
            return
        if "imasdef" in dir(imas) and self.__backend_id == imas.imasdef.MEMORY_BACKEND:
            return
        self.__db_entry.close()

    def initialize(self, sandbox_dir: str, db_name:str, backend_id):

        self.__pulse = 1
        self.__run = 1
        self.__backend_id = backend_id
        self.__db_name = db_name
        self.__sandbox_dir = sandbox_dir

        Path(sandbox_dir, 'tmp', '3', '0').mkdir(parents=True, exist_ok=True)

        self.__db_entry = imas.DBEntry( # pylint: disable=no-member
                                 self.__backend_id,  # backend_id
                                 self.__db_name,     # db_name
                                 self.__pulse,        # shot / pulse
                                 self.__run,         # run
                                 user_name=self.__sandbox_dir,  # AL hack to use sandbox dir
         )

        status, _not_used = self.__db_entry.create()
        if status != 0:
            raise Exception(
                f"Error creating the temporary DB: "
                f"backend={self.__backend_id} "
                f"name={self.__db_name} "
                f"pulse={self.__pulse} "
                f"run={self.__run} "
                f"dir={self.__sandbox_dir}" )

        self.__close_db()

    def prepare_data(self, ids_name):
        occurrence = self.__get_occurrence( ids_name )

        ids_description = IDSDescription(self.__db_entry, ids_name, occurrence)
        return ids_description

    def save_data(self, ids_description:IDSDescription, legacy_ids):
        self.__open_db()
        self.__db_entry.put( legacy_ids, ids_description.occurrence )
        self.__close_db()

    def read_data(self, ids_description:IDSDescription):
        self.__open_db()
        legacy_ids = self.__db_entry.get( ids_description.ids_type, ids_description.occurrence )
        self.__close_db()

        return legacy_ids

    def release_data(self, ids_name):
        self.__release_occurrence( ids_name )

    def finalize(self):
        try:
            self.__db_entry.close(erase=True)
        except:
            self.__db_entry.close()
