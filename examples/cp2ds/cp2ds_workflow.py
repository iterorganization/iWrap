import sys

import imas,os

from core2dist.actor import core2dist
from core2dist.common.runtime_settings import RunMode, DebugMode



class ExampleWorkflowManager:

    def __init__(self):

        self.actor_cp2ds = core2dist()
        print( self.actor_cp2ds.unique_id )
        print(self.actor_cp2ds.actor_description['data_type'])
        print(self.actor_cp2ds.code_description['implementation']['subroutines'])
        print(self.actor_cp2ds.code_description['implementation']['include_path'])
        print(self.actor_cp2ds.code_description['implementation']['data_type'])
        for arg in self.actor_cp2ds.code_description['implementation']['subroutines']['main']['arguments']:
            print(arg)
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

        # # # # # # # # Initialization of ALL actors  # # # # # # # # #
        actor_run_mode = os.getenv( 'ACTOR_RUN_MODE', 'NORMAL')
        runtime_settings = None
        if actor_run_mode == 'STANDALONE':
            print('Running STANDALONE version.')
            runtime_settings = self.actor_cp2ds.get_runtime_settings()
            runtime_settings.run_mode = RunMode.STANDALONE

        code_parameters = self.actor_cp2ds.get_code_parameters()
        self.actor_cp2ds.initialize(runtime_settings=runtime_settings, code_parameters=code_parameters)

    def execute_workflow(self):
        # READ INPUT IDSS FROM LOCAL DATABASE
        print('=> Read input IDSs')
        input_core_profiles = self.input_entry.get('core_profiles')

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')

        output_distribution_sources = self.actor_cp2ds(input_core_profiles)
        
        
        # SAVE IDSS INTO OUTPUT FILE
        print('=> Export output IDSs to local database')
        self.output_entry.put(output_distribution_sources)
        print('Done exporting.')


    def end_workflow(self):
        
        # Finalize ALL actors 
        self.actor_cp2ds.finalize()


        output_ids = self.output_entry.get('distribution_sources')

        with open( 'wf_output.txt', 'w' ) as file:
            file.write( str(output_ids.time) )



        #other finalizastion actions
        self.input_entry.close()
        self.output_entry.close()



manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()






