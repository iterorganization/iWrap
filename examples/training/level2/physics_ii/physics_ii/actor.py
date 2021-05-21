from iwrap.settings.code_description import Argument

from physics_ii.common.job_settings import JobSettings
from physics_ii.binding.binder import PhysicsIIBinder
from physics_ii.parameters import Parameters


class PhysicsIIActor:

    def __init__(self):
        self.job_settings = JobSettings()
        self.arguments = Argument( {'name': 'equilibrium0', 'type': 'equilibrium', 'intent': Argument.IN} ), \
                         Argument( {'name': 'equilibrium1', 'type': 'equilibrium', 'intent': Argument.OUT} )
        self.parameters = Parameters()
        self.binder = PhysicsIIBinder()


	 # # #  Actor lifecycle methods # # #
    def initialize(self):
        self.parameters.read()
        self.parameters.validate()

        self.binder.initialize(self.arguments, self.parameters)

        pass


    def __call__(self, *args):
        return self.run(*args)

    def run(self, *args):
        out = self.binder.call_native_code(*args)
        return out



    def finalize(self):
        print('PhysicsII:ACTOR:: FINALIZE')
        pass


