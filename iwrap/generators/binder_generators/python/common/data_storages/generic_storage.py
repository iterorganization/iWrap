import os
from abc import ABC, abstractmethod
import imas


class GenericIDSStorage(ABC):




    @classmethod
    @abstractmethod
    def prepare_data(cls, ids_name):
        ...

    @classmethod
    @abstractmethod
    def save_data(cls, ids_name, ids_data):
        ...

    @classmethod
    @abstractmethod
    def read_data(cls, ids_name, ids_data):
        ...

    @classmethod
    def initialize(cls, db_name:str, backend_id):
        ...




class LegacyIDSStorage( GenericIDSStorage ):
    __occ_dict = {}

    __db_entry = None

    @classmethod
    def __get_occurrence(cls, ids_name):
        occ = 1 + cls.__occ_dict.get(ids_name, -1 )
        cls.__occ_dict[ids_name] = occ
        return occ

    @classmethod
    def __release_occurrence(cls, ids_name):
        occ = cls.__occ_dict.get(ids_name, 1 )
        cls.__occ_dict[ids_name] = occ - 1

    @classmethod
    def __create_cache_db(cls, db_name:str, backend_id, shot, run):

        db_entry = imas.DBEntry( backend_id=backend_id,   # pylint: disable=no-member
                                 db_name=db_name,
                                 shot=shot,
                                 run=run )

        # TODO: correct after solvinf IMAS-3935 and IMAS-3936
        try:
            _not_used, status = db_entry.open()
            if status != 0:
                db_entry.create()
        except:
            db_entry.create()



        return db_entry

    @classmethod
    def initialize(cls, db_name:str, backend_id):
        shot = os.getpid() % 200_000  #MDS BE Limitation
        run = 1
        cls.__db_entry =  cls.__create_cache_db(db_name, backend_id, shot, run)

    @classmethod
    def prepare_data(cls, ids_name):
        occurrence = cls.__get_occurrence( ids_name )
        return cls.__db_entry, occurrence

    @classmethod
    def save_data(cls, ids_name, ids_occurrence, legacy_ids):
        cls.__db_entry.put( legacy_ids, ids_occurrence )

    @classmethod
    def read_data(cls, ids_name, ids_occurrence):
        legacy_ids = cls.__db_entry.get( ids_name, ids_occurrence )
        return legacy_ids

    @classmethod
    def release_data(cls, ids_name):
        cls.__release_occurrence( ids_name )

    @classmethod
    def finalize(cls):
        cls.__db_entry.close()
