import sys
import tempfile
from datetime import datetime

import jinja2

from iwrap.generation.generator import ActorGenerator
from iwrap.settings.project import ProjectSettings


class FortranWrapperGenerator( ActorGenerator ):

    TEMPLATE_SUBDIR = 'fortran_wrapper'
    FILES_TO_BE_COPIED = ('src/','','')

    def __init__(self):
        self.temp_dir: tempfile.TemporaryDirectory = None
        self.jinja_env: jinja2.Environment = None
        self.actor_settings: ProjectSettings = None
        pass

    def init(self, actor_settings: ProjectSettings, generation_env: dict):
        self.actor_settings = actor_settings
        self.temp_dir = generation_env['temp_dir']
        self.jinja_env = generation_env['jinja_env']

    def generate(self):
        dictionary = self.actor_settings.code_description.to_dict()
        template = self.jinja_env.get_template( 'fortran_wrapper/Makefile.jinja2' )
        print( template.stream( dictionary).dump(sys.stdout) )
        pass

    def build(self):
        pass

    def install(self):
        pass

    def cleanup(self):
        pass

    def get_code_signature(self) -> str:
        return 'To be Defined : [ ' + str(datetime.now().time()) + ' ]'
