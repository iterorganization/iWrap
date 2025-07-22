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
        if self.__backend_id == imas.ids_defs.MEMORY_BACKEND:
            return
        
        try:
            self.__db_entry.open()
        except Exception as e:
            raise RuntimeError(
                f"Error opening the temporary DB:\n"
                f"  backend = {self.__backend_id}\n"
                f"  name    = {self.__db_name}\n"
                f"  pulse   = {self.__pulse}\n"
                f"  run     = {self.__run}\n"
                f"  dir     = {self.__sandbox_dir}\n"
                f"Original exception: {e}"
            ) from e

    def __close_db(self):
        if self.__backend_id == imas.ids_defs.MEMORY_BACKEND:
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
                                 backend_id=self.__backend_id,  # backend_id
                                 db_name=self.__db_name,     # db_name
                                 pulse=self.__pulse,        # shot / pulse
                                 run=self.__run,         # run
                                 user_name=self.__sandbox_dir,  # AL hack to use sandbox dir
         )
        try:
            self.__db_entry.create()
        except Exception as e:
            raise RuntimeError(
                f"Error creating the temporary DB:\n"
                f"  backend = {self.__backend_id}\n"
                f"  name    = {self.__db_name}\n"
                f"  pulse   = {self.__pulse}\n"
                f"  run     = {self.__run}\n"
                f"  dir     = {self.__sandbox_dir}\n"
                f"Original exception: {e}"
            ) from e

        self.__close_db()

    def prepare_data(self, ids_name):
        occurrence = self.__get_occurrence( ids_name )
        self.__open_db()
        ids_description = IDSDescription(self.__db_entry, ids_name, occurrence)
        self.__close_db()
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
