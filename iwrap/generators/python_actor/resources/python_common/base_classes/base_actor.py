from abc import ABC

from ..job_settings import JobSettings
from ..fortran_binding.binder import FortranBinder
from ..code_parameters import CodeParameters


class ActorBaseClass( ABC ):

    def __init__(self, actor_dir, code_name):
        self.runtime_settings = JobSettings()
        self.formal_arguments = []
        self.code_parameters = CodeParameters()
        self.binder = FortranBinder( actor_dir, self.__class__.__name__, code_name )

    # # #  Actor lifecycle methods # # #

    def initialize(self):
        self.code_parameters.read()
        self.code_parameters.validate()

        self.binder.initialize( self.runtime_settings,
                                self.formal_arguments, self.code_parameters )

        pass

    def __call__(self, *args):
        return self.run( *args )

    def run(self, *args):
        out = self.binder.call_native_code( *args )
        return out

    def finalize(self):
        pass
