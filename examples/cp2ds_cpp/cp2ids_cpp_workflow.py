import sys

import imas,os

from core2dist_cpp.actor import core2dist_cpp
from core2dist_cpp.python_common.job_settings import RunMode, DebugMode



class ExampleWorkflowManager:

    def __init__(self):

        self.actor_cp2ds = core2dist_cpp()
        self.input_entry = None
        self.output_entry = None

    def init_workflow(self):

        # INPUT/OUTPUT CONFIGURATION
        shot                = 134174
        run_in              = 37
        input_user_or_path  = 'public'
        input_database      = 'iter'
        run_out             = 10
        output_user_or_path = os.getenv('USER')
        output_database     = input_database

        # OPEN INPUT DATAFILE TO GET DATA FROM IMAS SCENARIO DATABASE
        print('=> Open input datafile')
        self.input_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,input_database,shot,run_in,input_user_or_path)
        self.input_entry.open()
        
        # CREATE OUTPUT DATAFILE
        print('=> Create output datafile')
        self.output_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,output_database,shot,run_out,output_user_or_path)
        self.output_entry.create()

        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        #
        #self.actor_cp2ds.runtime_settings.run_mode = RunMode.STANDALONE
        #self.actor_cp2ds.runtime_settings.debug_mode = DebugMode.ATTACH
        self.actor_cp2ds.initialize()
    
    def execute_workflow(self):
        # READ INPUT IDSS FROM LOCAL DATABASE
        print('=> Read input IDSs')
        input_core_profiles = self.input_entry.get('core_profiles')
        print(input_core_profiles.time)

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')

        output_distribution_sources = self.actor_cp2ds(input_core_profiles)

        print( output_distribution_sources.time )
        
        
        # SAVE IDSS INTO OUTPUT FILE
        print('=> Export output IDSs to local database')
        self.output_entry.put(output_distribution_sources)
        print('Done exporting.')


    def end_workflow(self):
        
        # Finalize ALL actors 
        self.actor_cp2ds.finalize()
        
        #other finalizastion actions
        self.input_entry.close()
        self.output_entry.close()



manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()






