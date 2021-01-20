from iwrap.job_settings import SandboxSettings
from abcd.actor import ABCDActor
from xyz.actor import XYZActor


class ExampleScenarioManager:
    
    def __init__(self):
        self.actor_abcd = ABCDActor()
        self.actor_xyz = XYZActor()


    def init_scenario(self):
        # # # # # # # # Initialization of ABCD actor  # # # # # # # #
        #mpi
        self.actor_abcd.job_settings.mpi.nodes = 5
        
        #sandbox
        self.actor_abcd.job_settings.sandbox.lifetime = SandboxSettings.LIFETIME_SCENARIO
        self.actor_abcd.job_settings.sandbox.clean_up = False
 
        self.actor_abcd.initialize() 
        
        # # # # # # # # Initialization of XYZ actor  # # # # # # # #
        self.actor_xyz.parameters.code_parameters = '/path/to/XML/file'
        self.actor_xyz.parameters.default_parameters = '/path/to/XML/file'
        self.actor_xyz.parameters.schema = '/path/to/XML/file'
        
        self.actor_xyz.initialize() 
    
    def execute_scenario(self):

        abcd_in_args = self.actor_abcd.in_arguments
        abcd_in_args.in_arg1.value = 'fake_ids_object1'
        abcd_in_args.in_arg2.value = 'fake_ids_object2'
        
        abcd_out_args = self.actor_abcd.run(abcd_in_args) 
        

        for i in range(1,5):
            xyz_out_args = self.actor_xyz.run(abcd_out_args) 
            
            
        xyz_out_args = self.actor_abcd.run(xyz_out_args) 

    def end_scenario(self):
        self.actor_abcd.finalize() 
        self.actor_xyz.finalize() 


manager = ExampleScenarioManager()

manager.init_scenario()
manager.execute_scenario()
manager.end_scenario()



