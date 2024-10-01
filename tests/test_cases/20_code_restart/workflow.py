import os
from os.path import exists as file_exists

from utils import get_actor_instance, get_test_equilibrium


class ExampleWorkflowManager:
    def __init__(self):
        self.actor = get_actor_instance(actor_name="codeRestart", code_language="fortran")
        self.input_entry = None
        self.output_entry = None

    def init_workflow(self):
        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        runtime_settings = None
        actor_run_mode = os.getenv("ACTOR_RUN_MODE", "NORMAL")
        if actor_run_mode == "STANDALONE":
            print("Running STANDALONE version.")
            runtime_settings = self.actor.get_runtime_settings()
            runtime_settings.run_mode = actor_run_mode

        self.actor.initialize(runtime_settings=runtime_settings)

        # restore the code state if file keeping state exists
        if file_exists("test.out"):
            with open("test.out", "r") as file:
                code_state = file.readline()
                file.seek(0)

            print("Starting from the saved code state: ", code_state)
            self.actor.set_state(code_state.strip())
        else:
            print("Starting from 1")

    def execute_workflow(self):
        # READ INPUT IDS
        input_equilibrium = get_test_equilibrium()

        # EXECUTE PHYSICS CODE
        print("=> Execute physics code")
        self.output_equilibrium = self.actor(input_equilibrium)

    def end_workflow(self):
        code_state = self.actor.get_state()
        print("RETURNED CODE STATE: ", code_state)

        # save code state to file
        with open("test.out", "w") as file:
            file.write(f"{str(code_state).strip()}\n")
            file.write(f"{self.output_equilibrium.code.output_flag[0]:.2f}\n")
            file.write(str(self.output_equilibrium.time))

        # Finalize ALL actors
        self.actor.finalize()


manager = ExampleWorkflowManager()

# make sure that file keeping state does not exist
if file_exists("test.out"):
    os.remove("test.out")

# the first run of workflow (starting from 1)
print(" THE FIRST RUN OF THE WORKFLOW ".center(80, "="))
manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()

# the second run of workflow (starting from saved state)
print(" THE SECOND RUN OF THE WORKFLOW ".center(80, "="))
manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()
