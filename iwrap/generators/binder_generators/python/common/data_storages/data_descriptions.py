import imas

class IDSDescription:

    @property
    def uri(self):
        uri = f'imas:{self.backend_id}' \
              f'?user={self.user};database={self.database};version={self.version};'\
              f'shot={self.shot};run={self.run};idx={self.idx}' \
              f'#{self.ids_type}:{self.occurrence}'
        return uri

    def __init__(self, db_entry, ids_name, occurrence):
        self.backend_id = db_entry.backend_id
        self.user = db_entry.user_name
        self.database = db_entry.db_name
        self.version = db_entry.dd_version[0] if db_entry.dd_version else ""
        try:
            self.pulse = db_entry.pulse
        except AttributeError:
            self.pulse = db_entry.shot
        self.run = db_entry.run
        self.idx = db_entry._dbe_impl._db_ctx.ctx
        self.ids_type = ids_name
        self.occurrence = occurrence
        self.intent = None

    def save(self, stream):
        stream.write( "------- IDS -------\n" )
        stream.write( self.ids_type )
        stream.write( "\n" )
        stream.write( str( self.pulse ) )
        stream.write( "\n" )
        stream.write( str( self.run ) )
        stream.write( "\n" )
        stream.write( str( self.occurrence ) )
        stream.write( "\n" )
        stream.write( str( self.backend_id ) )
        stream.write( "\n" )
        stream.write( str( self.idx ) )
        stream.write( "\n" )
        stream.write( self.database )
        stream.write( "\n" )
        stream.write( self.user )
        stream.write( "\n" )
        stream.write( self.version )
        stream.write( "\n" )

    def convert_to_native_type(self):
        pass
