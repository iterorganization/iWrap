import os

from utils import get_actor_instance, get_test_equilibrium


class ExampleWorkflowManager:
    def __init__(self):
        self.actor = get_actor_instance()

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
        # EXECUTE PHYSICS CODE
        print("=> Execute physics code")
        self.actor()

        print("=> Actor build info")
        print(f'IWRAP_VERSION:   {self.actor.build_info.get("iwrap_version")}')
        print(f'IMAS_VERSION:    {self.actor.build_info.get("imas_version")}')
        print(f'IMAS_PREFIX:     {self.actor.build_info.get("imas_prefix")}')
        print(f'AL_VERSION:      {self.actor.build_info.get("al_version")}')
        print(f'GENERATION_DATE: {self.actor.build_info.get("generation_date")}')

    def end_workflow(self):
        with open("test.out", "w") as file:
            file.write(
                f'OUT_IWRAP_VERSION=iwrap {self.actor.build_info.get("iwrap_version")}\n'
            )
            file.write(
                f'OUT_IMAS_VERSION={self.actor.build_info.get("imas_version")}\n'
            )
            file.write(f'OUT_IMAS_PREFIX={self.actor.build_info.get("imas_prefix")}\n')
            file.write(f'OUT_AL_VERSION={self.actor.build_info.get("al_version")}\n')


manager = ExampleWorkflowManager()

manager.init_workflow()
manager.execute_workflow()
manager.end_workflow()
