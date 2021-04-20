import tempfile

from iwrap.generation.generator import ActorGenerator
from iwrap.generators.python_actor.fortran_wrapping import FortranWrapperGenerator
from iwrap.settings.code_description import CodeDescription

import jinja2
import sys

from iwrap.settings.project import ProjectSettings


class PythonActorGenerator( ActorGenerator ):

    def __init__(self):
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.wrapper_generator = FortranWrapperGenerator()

    def init(self):
        self.jinja_env = jinja2.Environment(
            loader=jinja2.PackageLoader( 'iwrap.generators.python_actor', 'resources' ),
            autoescape=jinja2.select_autoescape( ['html', 'xml'] )
        )

        self.wrapper_generator = FortranWrapperGenerator()

    def generate(self):
        self.temp_dir = tempfile.TemporaryDirectory()
        print( 'created temporary directory', self.jinja_env.list_templates() )

        generation_env = {'temp_dir': self.temp_dir, 'jinja_env': self.jinja_env}
        actor_settings = ProjectSettings.get_settings()

        self.wrapper_generator.init( actor_settings, generation_env )

        self.wrapper_generator.generate()

    def build(self):
        pass

    def install(self):
        pass

    def cleanup(self):
        self.temp_dir.clenaup()

    def get_code_signature(self) -> str:
        return self.wrapper_generator.get_code_signature()
