import sys

import imas,os

from dummy_actor.actor import dummy_actor
from dummy_actor.common.runtime_settings import RunMode, DebugMode

class ExampleWorkflowManager:

    def __init__(self):
        self.actor_dummy_actor = dummy_actor()

    def init_workflow(self):

        print('=> Workflow initialization')
        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        runtime_settings = None
        actor_run_mode = os.getenv( 'ACTOR_RUN_MODE', 'NORMAL')
        if actor_run_mode == 'STANDALONE':
            print('Running STANDALONE version.')
            runtime_settings = self.actor_dummy_actor.get_runtime_settings()
            runtime_settings.run_mode = RunMode.STANDALONE

        code_parameters = self.actor_dummy_actor.get_code_parameters()

        code_parameters = self.actor_dummy_actor.get_code_parameters()
        xxx = code_parameters.get_parametr_value('parameters/multiplication_factor')
        print(xxx)
        code_parameters.set_parametr_value( 'parameters/multiplication_factor', 0.5 )
        xxx = code_parameters.get_parametr_value( 'parameters/multiplication_factor' )
        print( xxx )
        code_parameters.parameters_path = 'input/input_physics2.xml'
        self.actor_dummy_actor.initialize(runtime_settings=runtime_settings, code_parameters=code_parameters)
        print( code_parameters.parameters )

        code_parameters.parameters_path = 'input/input_physics.xml'
        self.actor_dummy_actor.initialize(runtime_settings=runtime_settings, code_parameters=code_parameters)
        print(code_parameters.parameters)

    def execute_workflow(self):

        # EXECUTE PHYSICS CODE
        print('=> Execute actor code')

        input_equilibrium = imas.equilibrium()
        output_equilibrium = self.actor_dummy_actor(input_equilibrium)

    def end_workflow(self):

        print('=> Workflow finalization')
        # Finalize ALL actors 
        self.actor_dummy_actor.finalize()

        with open( 'wf_output.txt', 'w' ) as file:
            file.write( 'SUCCESS' )
        
        #other finalization actions
        ...



manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()






