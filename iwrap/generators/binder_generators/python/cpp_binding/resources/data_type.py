import logging
import ctypes
from abc import ABC
from typing import Set

import imas
from .data_storages import IDSConverter
from .data_storages.generic_storage import LegacyIDSStorage

from ..common.definitions import Argument
from ..common.runtime_settings import IdsStorageSettings


class LegacyIDSConverter( IDSConverter, ctypes.Structure ):
    # Class logger
    __logger = logging.getLogger( __name__ + "." + __qualname__ )

    __data_storage = LegacyIDSStorage

    _fields_ = (("ids_name_", ctypes.c_byte * 132),
                ("shot", ctypes.c_int),
                ("run", ctypes.c_int),
                ("occurrence", ctypes.c_int),
                ("idx", ctypes.c_int),
                ("machine_", ctypes.c_byte * 132),
                ("user_", ctypes.c_byte * 132),
                ("version_", ctypes.c_byte * 132),
                )

    @classmethod
    def initialize(cls, is_standalone: bool, storage_settings: IdsStorageSettings) -> None:

        db_name = storage_settings.db_name
        if is_standalone:
            backend_id = storage_settings.persistent_backend
        else:
            backend_id = storage_settings.backend
        cls.__data_storage.initialize(db_name,  backend_id)

    @classmethod
    def finalize(cls) -> None:
        cls.__data_storage.finalize()

    @classmethod
    def data_type(cls) -> str:
        return 'legacy'

    @classmethod
    def code_languages(cls) -> Set[str]:
        return {'cpp', 'fortran'}

    def __init__(self, ids_name, intent, ids_value):

        if intent == Argument.IN:
            self.value = ids_value
            if ids_name != self.value.__name__:
                raise RuntimeError( "ERROR! Argument passed doesn't match arguments list: ", ids_name, ' vs ',
                                    self.value.__name__ )
        else:
            self.value = None

        self.intent = intent
        self.ids_name = ids_name

        self.occurrence = 0

        self.db_entry = None
        # these fields are redundant


    def convert_to_native_type(self):
        # check conflicting occurrences and store data

        self.db_entry, self.occurrence =  self.__data_storage.prepare_data(self.ids_name)

        self.idx = self.db_entry.db_ctx
        self.shot = self.db_entry.shot
        self.run = self.db_entry.run
        self.machine = self.db_entry.db_name
        self.user = self.db_entry.user_name
        self.version = self.db_entry.data_version

        # store input data
        if self.intent == Argument.IN:
            self.db_entry.put( self.value, self.occurrence )

        return ctypes.byref( self )

    def convert_to_actor_type(self):
        ids = self.db_entry.get( self.ids_name, self.occurrence )
        return ids

    def release(self):
        self.__data_storage.release_data(self.ids_name)

    @property
    def ids_name(self):
        return ''.join( (chr( x ) for x in self.ids_name_[:]) ).strip()

    @ids_name.setter
    def ids_name(self, ids_name_):
        self.ids_name_[:] = len( self.ids_name_ ) * [ord( ' ' )]
        self.ids_name_[:len( ids_name_ )] = [ord( x ) for x in ids_name_]

    @property
    def machine(self):
        return ''.join( (chr( x ) for x in self.machine_[:]) ).strip()

    @machine.setter
    def machine(self, machine_):
        self.machine_[:] = len( self.machine_ ) * [ord( ' ' )]
        self.machine_[:len( machine_ )] = [ord( x ) for x in machine_]

    @property
    def user(self):
        return ''.join( (chr( x ) for x in self.user_[:]) ).strip()

    @user.setter
    def user(self, user_):
        self.user_[:] = len( self.user_ ) * [ord( ' ' )]
        self.user_[:len( user_ )] = [ord( x ) for x in user_]

    @property
    def version(self):
        return ''.join( (chr( x ) for x in self.version_[:]) ).strip()

    @version.setter
    def version(self, version_):
        self.version_[:] = len( self.version_ ) * [ord( ' ' )]
        self.version_[:len( version_ )] = [ord( x ) for x in version_]

    def save(self, stream):
        stream.write( "------- IDS -------\n" )
        stream.write( self.ids_name )
        stream.write( "\n" )
        stream.write( str( self.shot ) )
        stream.write( "\n" )
        stream.write( str( self.run ) )
        stream.write( "\n" )
        stream.write( str( self.occurrence ) )
        stream.write( "\n" )
        stream.write( str( self.idx ) )
        stream.write( "\n" )
        stream.write( self.machine )
        stream.write( "\n" )
        stream.write( self.user )
        stream.write( "\n" )
        stream.write( self.version )
        stream.write( "\n" )
