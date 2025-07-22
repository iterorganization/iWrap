import sys

import imas, os

from utils import (
    get_actor_instance,
    get_test_equilibrium,
    get_test_core_profiles,
)


class ExampleWorkflowManager:
    def __init__(self):
        self.actor = get_actor_instance(actor_name="parallel_mpi", code_language="fortran")
        self.input_entry = None
        self.output_entry = None

    def init_workflow(self):
        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        #
        runtime_settings = self.actor.get_runtime_settings()
        actor_run_mode = os.getenv("ACTOR_RUN_MODE", "NORMAL")
        if actor_run_mode == "STANDALONE":
            print("Running STANDALONE version.")
            runtime_settings.run_mode = "STANDALONE"
        else:
            print("MPI is always run as STANDALONE. (Even if NORMAL was selected)")

        runtime_settings.mpi.mpi_processes = 3
        self.actor.initialize(runtime_settings=runtime_settings)

    def execute_workflow(self):
        # READ INPUT IDSS FROM LOCAL DATABASE
        print("=> Read input IDSs")
        core_profiles_in = get_test_core_profiles()

        # EXECUTE PHYSICS CODE
        print("=> Execute physics code")

        self.distribution_sources_out = self.actor(core_profiles_in)

    def end_workflow(self):
        # Finalize ALL actors
        self.actor.finalize()

        # SAVE WORKFLOW RESULT
        with open("test.out", "w") as file:
            file.write(f"{self.distribution_sources_out.time.value}")


manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()
