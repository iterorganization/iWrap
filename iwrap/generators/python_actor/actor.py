import shutil
import tempfile

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generators.python_actor.fortran_wrapping import FortranWrapperGenerator
from iwrap.settings.code_description import CodeDescription

import jinja2
import sys

from iwrap.settings.project import ProjectSettings


class ActorScriptGenerator( ActorGenerator ):

    def __init__(self):
        pass

    def init(self, **kwargs):
        pass

    def copy_file(self):
        src = self.jinja_env.loader.provider.module_path
        dst = None
        shutil.copy(src, dst)

    def generate(self):
        src = self.jinja_env.loader.provider.module_path
        dst = None
        shutil.copytree(src, dst, copy_function=self.copy_file)

    def build(self):
        pass

    def install(self):
        pass

    def cleanup(self):
        pass

    def get_code_signature(self) -> str:
        pass