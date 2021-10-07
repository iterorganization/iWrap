from abc import ABC

from ..runtime_settings import RuntimeSettings
from ..fortran_binding.binder import CBinder
from ..code_parameters import CodeParameters


class ActorBaseClass( ABC ):

    def __init__(self, actor_dir, native_language, code_name, is_mpi_code):
        self.runtime_settings = RuntimeSettings()
        self.arguments = []
        self.code_parameters = CodeParameters()
        self.actor_dir = actor_dir
        self.code_name = code_name
        self.native_language = native_language
        self.name = self.__class__.__name__
        self.is_mpi_code = is_mpi_code
        self.__binder = CBinder( actor=self )

    # # #  Actor lifecycle methods # # #

    def initialize(self):
        self.code_parameters.read()
        self.code_parameters.validate()

        self.__binder.initialize( )

        pass

    def __call__(self, *args):
        return self.run( *args )

    def run(self, *args):
        out = self.binder.call_native_code( *args )
        return out

    def finalize(self):
        pass
