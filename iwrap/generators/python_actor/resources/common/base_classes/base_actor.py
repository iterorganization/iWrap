from abc import ABC

from ..runtime_settings import RuntimeSettings
from ..fortran_binding.binder import FortranBinder
from ..code_parameters import CodeParameters


class ActorBaseClass( ABC ):

    def __init__(self, actor_dir, native_language, code_name, is_mpi_code):
        self.runtime_settings = RuntimeSettings()
        self.formal_arguments = []
        self.code_parameters = CodeParameters()
        self.binder = FortranBinder( actor_dir, self.__class__.__name__, native_language, code_name, is_mpi_code )

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
