import sys

import imas,os

from basic_methods_java.actor import basic_methods_java
from basic_methods_java.common.runtime_settings import RunMode, DebugMode



class ExampleWorkflowManager:

    def __init__(self):

        self.actor_basic_methods_java = basic_methods_java()
        self.input_entry = None
        self.output_entry = None
        self.distribution_sources = None
        self.core_profiles = None

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
        self.input_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,input_database,shot,run_in,input_user_or_path)
        self.input_entry.open()
        
        # CREATE OUTPUT DATAFILE
        print('=> Create output datafile')
        self.output_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,output_database,shot,run_out,output_user_or_path)
        self.output_entry.create()

        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        runtime_settings = None
        actor_run_mode = os.getenv( 'ACTOR_RUN_MODE', 'NORMAL')
        if actor_run_mode == 'STANDALONE':
            print('Running STANDALONE version.')
        runtime_settings = self.actor_basic_methods_java.get_runtime_settings()
        runtime_settings.run_mode = RunMode.STANDALONE

        code_parameters = self.actor_basic_methods_java.get_code_parameters()

        core_profiles_in = self.input_entry.get( 'core_profiles' )
        self.distribution_sources = self.actor_basic_methods_java.initialize(core_profiles_in, runtime_settings=runtime_settings, code_parameters=code_parameters)
    
    def execute_workflow(self):

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')

        self.core_profiles = self.actor_basic_methods_java(self.distribution_sources)
        



    def end_workflow(self):
        
        # Finalize ALL actors 
        distribution_sources = self.actor_basic_methods_java.finalize(self.core_profiles)


        with open( 'wf_output.txt', 'w' ) as file:
            file.write( str(distribution_sources.time) )
        
        #other finalizastion actions
        self.input_entry.close()
        self.output_entry.close()



manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()






