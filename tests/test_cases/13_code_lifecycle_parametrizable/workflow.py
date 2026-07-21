import os

from utils import (
    get_actor_instance,
    get_test_equilibrium,
    get_test_core_profiles,
)


class ExampleWorkflowManager:
    def __init__(self):
        self.actor = get_actor_instance(actor_name="codeLifecycleParametrizable", code_language="fortran", parameters_format="xml")
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

        code_parameters = self.actor.get_code_parameters()
        code_parameters.set_parameter("parameters/multiplication_factor", 4.5)
        xxx = code_parameters.get_parameter("parameters/multiplication_factor")
        print(f"parameters/multiplication_factor = {xxx}")

        input_core_profiles = get_test_core_profiles()
        self.distribution_sources_in = self.actor.initialize(
            input_core_profiles,
            runtime_settings=runtime_settings,
            code_parameters=code_parameters,
        )

    def execute_workflow(self):
        # READ INPUT IDS
        self.input_equilibrium = get_test_equilibrium()

        # EXECUTE PHYSICS CODE
        print("=> Execute physics code")

        self.output_equilibrium = self.actor(self.input_equilibrium)

    def end_workflow(self):
        # Finalize ALL actors
        self.output_core_profiles = self.actor.finalize(self.distribution_sources_in)

        # SAVE WORKFLOW RESULT
        with open("test.out", "w") as file:
            file.write(f"{self.output_equilibrium.code.output_flag[0]:.2f}\n")
            file.write(str(self.output_equilibrium.time.value))


manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()
