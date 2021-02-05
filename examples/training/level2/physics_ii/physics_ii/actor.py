from iwrap.arguments import Argument
from iwrap.job_settings import JobSettings

from physics_ii.binding.binder import PhysicsIIBinder
from physics_ii.parameters import  Parameters


class PhysicsIIActor:
        
    def __init__(self):
        self.job_settings = JobSettings()
        self.in_arguments = Argument('equilibrium0', 'IDS', "equilibrium", Argument.IN),
        self.out_arguments = Argument('equilibrium1', "IDS", "equilibrium", Argument.OUT),
        self.parameters = Parameters()
        self.binder = PhysicsIIBinder()

  
	 # # #  Actor lifecycle methods # # #
    def initialize(self):
        self.parameters.read()
        self.parameters.validate()
        
        pass


    def __call__(self, *args):
        return self.run(*args)

    def run(self, *args):
        out = self.binder.call_native_code(*args, self.parameters)
        return out
 

    
    def finalize(self):
        print('PhysicsII:ACTOR:: FINALIZE')
        pass


