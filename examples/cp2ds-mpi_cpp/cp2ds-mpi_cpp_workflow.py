import sys

import imas,os

from core2dist_mpi_cpp.actor import core2dist_mpi_cpp
from core2dist_mpi_cpp.python_common.job_settings import RunMode, DebugMode



class ExampleWorkflowManager:

    def __init__(self):

        self.actor_cp2ds_mpi = core2dist_mpi_cpp()
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
        actor_run_mode = os.getenv( 'ACTOR_RUN_MODE', 'NORMAL')
        if actor_run_mode == 'STANDALONE':
            print('Running STANDALONE version.')
            self.actor_cp2ds_mpi.runtime_settings.run_mode = RunMode.STANDALONE

        self.actor_cp2ds_mpi.runtime_settings.mpi.number_of_processes = 3
        self.actor_cp2ds_mpi.initialize()
    
    def execute_workflow(self):
        # READ INPUT IDSS FROM LOCAL DATABASE
        print('=> Read input IDSs')
        input_core_profiles = self.input_entry.get('core_profiles')

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')

        output_distribution_sources = self.actor_cp2ds_mpi(input_core_profiles)

        # SAVE IDSS INTO OUTPUT FILE
        print('=> Export output IDSs to local database')
        self.output_entry.put(output_distribution_sources)
        print('Done exporting.')


    def end_workflow(self):
        
        # Finalize ALL actors 
        self.actor_cp2ds_mpi.finalize()


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






