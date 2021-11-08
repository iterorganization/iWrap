import sys

import imas,os

from loop.actor import loop
from loop.common.runtime_settings import RunMode, DebugMode



class ExampleWorkflowManager:

    def __init__(self):

        self.actor_loop = loop()
        self.output_entry = None
        self.equilibrium = None

    def init_workflow(self):
        # INPUT/OUTPUT CONFIGURATION
        shot                = 131024
        run_in              = 1
        input_user_or_path  = 'public'
        input_database      = 'iter'
        run_out             = 10
        output_user_or_path = os.getenv('USER')
        output_database     = input_database

        # OPEN INPUT DATAFILE TO GET DATA FROM IMAS SCENARIO DATABASE
        print('=> Open input datafile')
        input_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,input_database,shot,run_in,input_user_or_path)
        input_entry.open()
        
        # CREATE OUTPUT DATAFILE
        print('=> Create output datafile')
        self.output_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,output_database,shot,run_out,output_user_or_path)
        self.output_entry.create()

        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        actor_run_mode = os.getenv( 'ACTOR_RUN_MODE', 'NORMAL')
        if actor_run_mode == 'STANDALONE':
            print('Running STANDALONE version.')
            self.actor_loop.runtime_settings.run_mode = RunMode.STANDALONE

        #self.actor_loop.code_parameters.parameters= '/gss_efgw_work/scratch/g2bpalak/tmp/xml_new_location.xml'
        self.actor_loop.initialize()
        # READ INPUT IDSS FROM LOCAL DATABASE
        time_slice          = 200.
        print('=> Read input IDSs')
        self.equilibrium = input_entry.get_slice('equilibrium', time_slice, 1)

    
    def execute_workflow(self):

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')
        self.actor_loop.initialize()
        for i in range( 10 ):
            print(f' ITERATION: {i} '.center(50, '='))
            '''
            if i % 2 == 0:
                self.actor_loop.code_parameters.parameters_path = '/gss_efgw_work/scratch/g2bpalak/tmp/xml_new_location.xml'
            else:
                self.actor_loop.code_parameters.parameters_path = None
            '''

            self.equilibrium = self.actor_loop( self.equilibrium )

        self.actor_loop.finalize()

    def end_workflow(self):
        
        # Finalize ALL actors 
        self.actor_loop.finalize()

        # SAVE IDSS INTO OUTPUT FILE
        print( '=> Export output IDSs to local database' )
        self.output_entry.put( self.equilibrium )
        print( 'Done exporting.' )

        with open( 'wf_output.txt', 'w' ) as file:
            file.write( str(self.equilibrium.time) )
        
        #other finalizastion actions

        self.output_entry.close()



manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()






