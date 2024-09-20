import os

from utils import get_actor_instance, get_test_equilibrium


class ExampleWorkflowManager:
    def __init__(self):
        self.actor = get_actor_instance()

    def init_workflow(self):
        print("=> Workflow initialization")
        log_file = open("dummy_actor.log", "a")
        logger = self.actor.logging_config("debug", log_file)
        logger.info("test logging in init_workflow")

        # # # # # # # # Initialization of ALL actors  # # # # # # # #
        runtime_settings = None
        actor_run_mode = os.getenv("ACTOR_RUN_MODE", "NORMAL")
        if actor_run_mode == "STANDALONE":
            print("Running STANDALONE version.")
            runtime_settings = self.actor.get_runtime_settings()
            runtime_settings.run_mode = actor_run_mode

        self.actor.initialize(runtime_settings=runtime_settings)

    def execute_workflow(self):
        # EXECUTE PHYSICS CODE
        print("=> Execute actor code")
        self.actor()

    def end_workflow(self):
        print("=> Workflow finalization")
        # Finalize ALL actors
        self.actor.finalize()

        with open("test.out", "w") as file:
            file.write("SUCCESS")

        # other finalization actions
        ...


manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()
