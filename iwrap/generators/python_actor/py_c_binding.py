import tempfile

from iwrap.generation.generator import ActorGenerator
from iwrap.generators.python_actor.fortran_wrapping import FortranWrapperGenerator
from iwrap.settings.code_description import CodeDescription

import jinja2
import sys

from iwrap.settings.project import ProjectSettings


class PythonActorGenerator( ActorGenerator ):

    def __init__(self):
        pass

    def init(self):
        pass

    def generate(self):
        pass

    def build(self):
        pass

    def install(self):
        pass

    def cleanup(self):
        self.temp_dir.clenaup()


