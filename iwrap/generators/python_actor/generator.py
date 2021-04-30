import tempfile
from typing import Set

from iwrap.generation_engine.base_classes import ActorGenerator
from iwrap.generators.python_actor.fortran_wrapping import FortranWrapperGenerator
from iwrap.settings.code_description import CodeDescription

import jinja2
import sys

from iwrap.settings.project import ProjectSettings


class PythonActorGenerator(ActorGenerator):

    @property
    def name(self) -> str:
        return 'Python actor'

    @property
    def description(self) -> str:
        return 'Simple Python actor'

    @property
    def actor_data_types(self) -> Set[str]:
        return {'Legacy IDS', 'HDC IDS'}

    @property
    def code_data_types(self) -> Set[str]:
        return {'Legacy IDS', 'HDC IDS'}

    @property
    def code_languages(self) -> Set[str]:
        return {'Fortran', 'CPP'}

    def __init__(self):
        self.install_dir: str = None
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.wrapper_generator = FortranWrapperGenerator()

    def init(self):
        self.jinja_env = jinja2.Environment(
            loader=jinja2.PackageLoader('iwrap.generators.python_actor', 'resources'),
            autoescape=jinja2.select_autoescape(['html', 'xml'])
        )
        self.wrapper_generator = FortranWrapperGenerator()

    def generate(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        print('created temporary directory', self.jinja_env.list_templates())

        generation_env = {'temp_dir': self.temp_dir, 'jinja_env': self.jinja_env}
        actor_settings = ProjectSettings.get_settings()

        self.wrapper_generator.init(actor_settings, generation_env)

        self.wrapper_generator.generate()

    def build(self):
        pass

    def install(self):
        pass

    def cleanup(self):
        self.temp_dir.cleanup()

    def get_code_signature(self) -> str:
        return self.wrapper_generator.get_code_signature()
