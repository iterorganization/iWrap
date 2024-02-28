import sys

import imas,os

from os.path import exists as file_exists

from code_restart_cpp.actor import code_restart_cpp
from code_restart_cpp.common.runtime_settings import RunMode, DebugMode



class ExampleWorkflowManager:

    def __init__(self):
        self.actor_code_restart = code_restart_cpp()
        self.input_entry = None

    def init_workflow(self):
        # INPUT/OUTPUT CONFIGURATION
        shot                = 131024
        run_in              = 1
        input_user_or_path  = 'public'
        input_database      = 'iter'

        # OPEN INPUT DATAFILE TO GET DATA FROM IMAS SCENARIO DATABASE
        print('=> Open input datafile')
        self.input_entry = imas.DBEntry(imas.imasdef.MDSPLUS_BACKEND,input_database,shot,run_in,input_user_or_path)
        self.input_entry.open()

        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        runtime_settings = self.actor_code_restart.get_runtime_settings()
        #runtime_settings.debug_mode = DebugMode.ATTACH

        code_parameters = self.actor_code_restart.get_code_parameters()
        self.actor_code_restart.initialize(runtime_settings=runtime_settings, code_parameters=code_parameters)

        # restore the code state if file keeping state exists
        if file_exists('wf_output.txt'):
            with open( 'wf_output.txt', 'r' ) as file:
                contents = file.read()
                code_state = contents

            print( 'Starting from the saved code state: ', code_state )
            self.actor_code_restart.set_state(code_state)
        else:
            print( 'Starting from 0' )

    def execute_workflow(self):
        # READ INPUT IDSS FROM LOCAL DATABASE
        time_slice          = 200.
        print('=> Read input IDSs')
        input_equilibrium = self.input_entry.get_slice('equilibrium', time_slice, 1)

        # EXECUTE PHYSICS CODE
        print('=> Execute physics code')
        self.actor_code_restart(input_equilibrium)


    def end_workflow(self):
        code_state = self.actor_code_restart.get_state()
        print( 'RETURNED CODE STATE: ', code_state )

        # save code state to file
        with open( 'wf_output.txt', 'w' ) as file:
            file.write(  code_state.strip() )

        # Finalize ALL actors
        self.actor_code_restart.finalize()
        #other finalizastion actions
        self.input_entry.close()


if os.getenv('ACTOR_RUN_MODE', 'NORMAL') == 'STANDALONE':
    exit(0)

manager = ExampleWorkflowManager()

# make sure that file keeping state does not exist
if file_exists("wf_output.txt"):
    os.remove("wf_output.txt")

# the first run of workflow (starting from 0)
print( ' THE FIRST RUN OF THE WORKFLOW '.center( 80, '=' ) )
manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()

# the second run of workflow (starting from saved state)
print( ' THE SECOND RUN OF THE WORKFLOW '.center( 80, '=' ) )
manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()





