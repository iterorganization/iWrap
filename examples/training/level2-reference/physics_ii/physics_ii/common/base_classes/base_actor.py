from abc import ABC

from iwrap.settings.code_description import Argument

from physics_ii.common.job_settings import JobSettings
from physics_ii.binding.binder import PhysicsIIBinder
from physics_ii.code_parameters import CodeParameters



class ActorBaseClass(ABC):


    def __init__(self, code_name):

        self.runtime_settings = JobSettings()
        self.formal_arguments = []
        self.code_parameters = None
        self.binder = PhysicsIIBinder(self.__class__.__name__, code_name)


	 # # #  Actor lifecycle methods # # #
    def initialize(self):
        self.code_parameters.read()
        self.code_parameters.validate()

        self.binder.initialize(self.formal_arguments, self.code_parameters)

        pass


    def __call__(self, *args):
        return self.run(*args)

    def run(self, *args):
        out = self.binder.call_native_code(*args)
        return out



    def finalize(self):
        pass


