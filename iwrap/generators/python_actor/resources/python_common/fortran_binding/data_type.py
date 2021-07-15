import ctypes
import imas

from ..definitions import Argument

class LegacyIDS ( ctypes.Structure ):

    _fields_ = (("ids_name_", ctypes.c_byte * 132),
                ("shot", ctypes.c_int),
                ("run", ctypes.c_int),
                ("occurrence", ctypes.c_int),
                ("idx", ctypes.c_int),
                ("machine_", ctypes.c_byte * 132),
                ("user_", ctypes.c_byte * 132),
                ("version_", ctypes.c_byte * 132),
                )

    occ_dict = {}

    def __init__(self, db_entry:imas.DBEntry, formal_desc, ids_value):


        if formal_desc.intent == Argument.IN:
            self.value = ids_value
            if formal_desc.type != self.value.__name__:
                raise RuntimeError("ERROR! Argument passed doesn't match arguments list: ", formal_desc.type, ' vs ', self.value.__name__ )
            self.ids_name = self.value.__name__
        else:
            self.value = None

        self.intent = formal_desc.intent

        self.ids_name = formal_desc.type
        self.occurrence = 0

        self.db_entry = db_entry
        # these fields are redundant
        self.idx = db_entry.db_ctx
        self.shot = db_entry.shot
        self.run = db_entry.run
        self.machine = db_entry.db_name
        self.user = db_entry.user_name
        self.version = db_entry.data_version

    def convert_to_native_type(self):
        # check conflicting occurences and store data

        LegacyIDS.occ_dict[self.ids_name] = 1 + LegacyIDS.occ_dict.get( self.ids_name, -1 )
        self.occurrence = LegacyIDS.occ_dict[self.ids_name]
        # store input data
        if self.intent == Argument.IN:
            self.db_entry.put( self.value, self.occurrence )

        return ctypes.byref( self )

    def convert_to_actor_type(self):
        ids = self.db_entry.get( self.ids_name, self.occurrence )
        return ids


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

    def save(self, stream ):
        stream.write( "------- IDS -------\n" )
        stream.write( self.ids_name )
        stream.write( "\n" )
        stream.write( str(self.shot) )
        stream.write( "\n" )
        stream.write( str(self.run) )
        stream.write( "\n" )
        stream.write( str(self.occurrence ))
        stream.write( "\n" )
        stream.write( str(self.idx) )
        stream.write( "\n" )
        stream.write( self.machine)
        stream.write( "\n" )
        stream.write( self.user )
        stream.write( "\n" )
        stream.write( self.version )
        stream.write( "\n" )

