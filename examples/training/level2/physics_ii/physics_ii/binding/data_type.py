import ctypes
import imas

class IDSData:

    def __init__(self, db_entry:imas.DBEntry, ids, intent, occurrence):
        self.ids_ref = self.IDSRef(db_entry, ids, occurrence)
        self.value = ids
        self.intent = intent

    def to_args(self):
        arg = {}
        arg['value'] = self.value
        arg['cval'] = self.ids_ref
        arg['cref'] = ctypes.byref( self.ids_ref )
        arg['in'] = self.intent == 'in'
        arg['out'] =  self.intent == 'out'
        return arg


    # # # # IDSRef internal class # # # #
    class IDSRef( ctypes.Structure ):
        '''CPORef reference structure'''
        _fields_ = (("ids_name_", ctypes.c_byte * 132),
                    ("shot", ctypes.c_int),
                    ("run", ctypes.c_int),
                    ("occurrence", ctypes.c_int),
                    ("idx", ctypes.c_int),
                    ("machine_", ctypes.c_byte * 132),
                    ("user_", ctypes.c_byte * 132),
                    ("version_", ctypes.c_byte * 132),
                    )

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

        def __init__(self, db_entry:imas.DBEntry, ids, occurrence):
            # the important fieds
            # we use the temporary shot/run
            self.idx = db_entry.db_ctx
            # TODO can we get the IDS name from the ids object?
            # so that fc2k does not have to fill this in the template
            self.ids_name = ids.__name__
            self.occurrence = occurrence
            # these fields are redundant
            self.shot = db_entry.shot
            self.run = db_entry.run
            self.machine = db_entry.db_name
            self.user = db_entry.user_name
            self.version = db_entry.data_version
            pass