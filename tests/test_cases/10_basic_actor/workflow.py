import os

from utils import get_actor_instance, get_test_equilibrium


class ExampleWorkflowManager:
    def __init__(self):
        self.actor = get_actor_instance()
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

    def execute_workflow(self):
        # READ INPUT IDS
        print("=> Read input IDSs")
        self.input_equilibrium = get_test_equilibrium()

        # EXECUTE PHYSICS CODE
        print("=> Execute physics code")

        self.output_equilibrium = self.actor(self.input_equilibrium)

    def end_workflow(self):
        # Finalize ALL actors
        self.actor.finalize()

        # SAVE WORKFLOW RESULT
        with open("test.out", "w") as file:
            file.write(f"{self.output_equilibrium.code.output_flag[0]:.2f}\n")
            file.write(str(self.output_equilibrium.time))


manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()
